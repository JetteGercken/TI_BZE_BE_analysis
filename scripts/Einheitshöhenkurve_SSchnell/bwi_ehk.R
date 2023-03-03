library(data.table);
library(RODBC);
library(stringi);
library(ggplot2)
library(plotly);

# dmean: mittlerer durchmesser pro plot 
# dg grunflächenmittelstamm
# hg höhe grundflächenmittelstamm
# id_broken: kennzahl, gibt an ob baum gebrochen ist etc. --> Höhenkennziffer

ehk_sloboda <- function(spec, d_i, d_mean, d_g, h_g, id_broken) {
  k0 <- c(fi = 0.183, ta = 0.097, dgl = 0.24, ki = 0.29, lae = 0.074, bu = 0.032, ei = 0.102, alh = 0.122, aln = 0.032)
  k1 <- c(fi = 5.688, ta = 3.992, dgl = 6.033, ki = 1.607, lae = 3.692, bu = 6.04, ei = 3.387, alh = 5.04, aln = 4.24)
  k2 <- c(fi = 0.29, ta = 0.317, dgl = 0.33, ki = 0.388, lae = 0.342, bu = 0.367, ei = 0.488, alh = 0.47, aln = 0.461)
  
  h_mean <- (h_g - 1.3)/(exp(k0[tolower(spec)]*(1 - d_mean/d_g))*exp(k1[tolower(spec)]*(1/d_mean - 1/d_g))) + 1.3;

  h_pred <- (1.3 + (h_mean - 1.3)*exp(k0[tolower(spec)]*(1 - d_mean/d_i))*exp(k1[tolower(spec)]*(1/d_mean - 1/d_i)));

  # Reduction factor depending on whether crown or stem is broken or not
  if (length(id_broken) == length(d_i)) {
    f_red <- rep(1.0, length(d_i));
    f_red[which(id_broken == 0)] <- 1.0;
    f_red[which(id_broken == 1)] <- 1 - 2/h_pred[which(id_broken == 1)];
    f_red[which(id_broken == 2)] <- 1 - k2[tolower(spec[which(id_broken == 2)])];
  } else if (length(id_broken) == 1) {
    if (id_broken == 0) f_red <-  1.0
    else if (id_broken == 1) f_red <- 1 - 2/h_pred
    else if (id_broken == 2) f_red <- 1 - k2[tolower(spec)]
  }

  return(h_pred*f_red)
}

h_curtis <- function(spec, d) {
  b0 <- c(fi = 434.1235, bu = 382.0202, ta = 453.5538, ki = 359.7162, lae = 421.4473, dgl = 481.5531, ei = 348.3262);
  b1 <- c(fi = -65586.6915, bu = -51800.9382, ta = -81132.5221, ki = -42967.9947, lae = -60241.2948, dgl = -81754.2523, ei = -46547.3645);
  b2 <- c(fi = 3074967.1738, bu = 2374368.3254, ta = 4285801.5636, ki = 1763359.9972, lae = 2895409.6245, dgl = 4193121.2406, ei = 2119420.9444);

  return(b0[tolower(spec)] + b1[tolower(spec)]*1/d + b2[tolower(spec)]*1/d^2)
}

con <- odbcConnect("HR17",
                   uid = "sschnell",
                   pwd = rstudioapi::askForPassword("Database password"))

dt_spec <- data.table(sqlQuery(con,
                               "SELECT ICode AS Ba, Gattung, Art, ba_tarif, ba_ekh AS ba_ehk, ba_ln
                                FROM bwi.xyk.x_ba"));
dt_spec[, ':='(ba_ehk = stri_trim_both(ba_ehk),
               ba_tarif = stri_trim_both(ba_tarif),
               ba_ln = stri_trim_both(ba_ln))];

dt_trees <- data.table(sqlQuery(con,
                                "SELECT a.Tnr, a.Enr, a.Bnr, Pk, Azi, Hori, a.Ba, Bhd, Hoehe, M_Hoe, a.Kh, a.Bs, a.Bz, N_ha, N_ha_Hb
                                FROM [bwi].[dat].[c3z_wzp] a
                                INNER JOIN bwi.dat.c3z_baeume b
                                ON a.Tnr = b.Tnr AND a.Enr = b.Enr AND a.Bnr = b.Bnr
                                WHERE Bhd >= 70"));

dt_trees <- dt_spec[, list(Ba, ba_tarif, ba_ehk, ba_ln)][dt_trees, on = "Ba"];

dt_trees[is.na(Bs) & Bz == 1, Bs := 1];
dt_trees[is.na(Kh), Kh := 0];
dt_trees[Bs %in% c(0, 1, 3), bs_group := 1];
dt_trees[Bs %in% c(2, 4, 9), bs_group := 2];
dt_trees[, g_i := Bhd^2*pi/4];
dt_trees[is.na(M_Hoe) | Kh != 0, is_sample := FALSE];
dt_trees[!is.na(M_Hoe) & Kh == 0, is_sample := TRUE];

# First round group by species, layer and stand
dt_trees[bs_group == 1,
         ':='(d_mean_1 = mean(Bhd)/10,
              d_g_1 = sqrt(mean(g_i[is_sample])*4/pi)/10,
              h_g_1 = sum(M_Hoe[is_sample]*g_i[is_sample])/sum(g_i[is_sample])/10),
         by = list(Tnr, Enr, ba_tarif, Bs, Bz)];
dt_trees[bs_group == 2,
         ':='(d_mean_1 = mean(Bhd)/10,
              d_g_1 = sqrt(mean(g_i[is_sample])*4/pi)/10,
              h_g_1 = (sum(M_Hoe[is_sample]*g_i[is_sample])/sum(g_i[is_sample]))/10),
         by = list(Tnr, Enr, ba_ln, Bs, Bz)];

# Second round group by layer and stand
dt_trees[bs_group == 1,
         ':='(d_mean_2 = mean(Bhd)/10,
              d_g_2 = sqrt(mean(g_i[is_sample])*4/pi)/10,
              h_g_2 = sum(M_Hoe[is_sample]*g_i[is_sample])/sum(g_i[is_sample])/10),
         by = list(Tnr, Enr, Bs, Bz)];
dt_trees[bs_group == 2,
         ':='(d_mean_2 = mean(Bhd)/10,
              d_g_2 = sqrt(mean(g_i[is_sample])*4/pi)/10,
              h_g_2 = (sum(M_Hoe[is_sample]*g_i[is_sample])/sum(g_i[is_sample]))/10),
         by = list(Tnr, Enr, Bs, Bz)];

dt_trees[, ':='(d_mean = ifelse(is.na(d_g_1), d_mean_2, d_mean_1),
                d_g = ifelse(is.na(d_g_1), d_g_2, d_g_1),
                h_g = ifelse(is.na(h_g_1), h_g_2, h_g_1))];
dt_trees[, ':='(d_mean_1 = NULL, d_mean_2 = NULL,
                d_g_1 = NULL, d_g_2 = NULL,
                h_g_1 = NULL, h_g_2 = NULL)]

# Handle missing values due to not available height measurements for some cases
setkey(dt_trees, Tnr, Enr, Bnr);
dt_id_miss <- unique(dt_trees[is.na(d_g),list(Tnr, Enr)]);
dt_miss <- dt_trees[dt_id_miss,
                    list(Tnr, Enr, Bnr, ba_tarif, ba_ln, Bs, Bz, bs_group, g_i),
                    on = c("Tnr", "Enr")];
dt_miss[bs_group == 1,
        d_g_miss := sqrt(mean(g_i)*4/pi)/10,
        by = list(Tnr, Enr, ba_tarif, Bs, Bz)];
dt_miss[bs_group == 2,
        d_g_miss := sqrt(mean(g_i)*4/pi)/10,
        by = list(Tnr, Enr, ba_ln, Bs, Bz)];

dt_trees <- dt_miss[, list(Tnr, Enr, Bnr, d_g_miss)][dt_trees, on = c("Tnr", "Enr", "Bnr")]
dt_trees[is.na(d_g), d_g := d_g_miss];
dt_trees[, d_g_miss := NULL];
dt_trees[is.na(h_g), h_g := h_curtis(ba_tarif, d_g*10)/10];

dt_trees[, h_pred := ehk_sloboda(spec = ba_ehk,
                                 d_i = Bhd/10,
                                 d_mean = d_mean,
                                 d_g = d_g,
                                 h_g = h_g,
                                 id_broken = Kh)];



ggplot(dt_trees, aes(x = h_pred, y = M_Hoe/10)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0) +
  coord_equal() +
  facet_grid(Kh ~ ba_ehk)

dt_trees[, ':='(h_diff_th = (M_Hoe - Hoehe)/10,
                h_diff_se = (M_Hoe/10 - h_pred),
                h_diff_se_th = (Hoehe/10 - h_pred))];
dt_se <- dt_trees[!is.na(h_diff_se),
                  list(se_th = sqrt(sum(h_diff_th^2)/.N),
                       se_se = sqrt(sum(h_diff_se^2)/.N),
                       bias_se = mean(h_diff_se),
                       bias_th = mean(h_diff_th)),
                  keyby = list(ba_ehk, Kh, bs_group)]

setorder(dt_trees, ba_ehk, h_pred)
dt_trees[,
         id_group := cut(1:.N, seq.int(1, .N, 20), labels = FALSE, include.lowest = TRUE),
         by = ba_ehk]
dt_group <- dt_trees[, list(h_meas = mean(M_Hoe/10, na.rm = TRUE),
                            h_pred = mean(h_pred, na.rm = TRUE),
                            h_diff_se = mean(h_diff_se, na.rm = TRUE),
                            h_diff_th = mean(h_diff_th, na.rm = TRUE)),
                     by = list(ba_ehk, id_group)];

ggplot(dt_group[!is.na(id_group)], aes(h_pred, h_meas)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  coord_equal() +
  facet_wrap(. ~ ba_ehk, nrow = 4)

ggplot(dt_group[!is.na(id_group)], aes(h_pred, h_diff_th)) +
  geom_point() +
  geom_abline(slope = 0, intercept = 0) +
  facet_wrap(. ~ ba_ehk, ncol = 2)



# tnr <- 196; enr <- 1;
# bhd_max <- dt_trees[Tnr == tnr & Enr == enr, max(Bhd)]/10;
# h_max <- dt_trees[Tnr == tnr & Enr == enr, max(M_Hoe, na.rm = TRUE)]/10;
#
# dt_sub <- dt_trees[Tnr == tnr & Enr == enr];
# dt_ehk_parms <- dt_sub[,
#                        list(d_mean = max(d_mean), h_g = max(h_g), Bs = max(Bs)),
#                        by = list(ba_ehk, d_g, Kh)];
# dt_ehk <- dt_ehk_parms[,
#                        list(id = .GRP,
#                             Bs = Bs,
#                             bhd = seq(0, bhd_max, by = 1),
#                             h_ehk = ehk_sloboda(ba_ehk, seq(0, bhd_max, by = 1), d_mean, d_g, h_g, Kh),
#                             d_mean, h_g),
#                        by = list(ba_ehk, d_g, Kh)];

## Plotly Experimente

# p <- plot_ly() %>%
#   add_trace(data = dt_ehk,
#             x = ~bhd,
#             y = ~h_ehk,
#             name = 'h_ehk',
#             type = 'scatter',
#             mode = 'lines',
#             linetype = ~as.factor(id),
#             color = ~ba_ehk,
#             inherit = FALSE,
#             line = list(dash = 'dash')) %>%
#   add_trace(data = dt_sub,
#             x = ~Bhd/10,
#             y = ~M_Hoe/10,
#             type = 'scatter',
#             mode = 'markers',
#             color = ~ba_ehk,
#             symbol = ~as.factor(Bs),
#             marker = list(symbol = c('circle', 'triangel-down')),
#             text = ~paste('Bhd: ', Bhd/10,
#                           '<br>M_Hoe:', M_Hoe/10,
#                           '<br>ba_ehk:', ba_ehk,
#                           '<br>Kh:', Kh,
#                           '<br>Bs:', Bs),
#             inherit = FALSE) %>%
#   add_trace(data = dt_sub,
#             x = ~Bhd/10,
#             y = ~h_pred,
#             name = 'h_pred',
#             type = 'scatter', mode = 'markers',
#             color = ~ba_ehk,
#             symbol = ~as.factor(Bs),
#             inherit = FALSE) %>%
#   layout(xaxis = list(title = 'Bhd [cm]',
#                       range = c(0, bhd_max + 10),
#                       zeroline = TRUE,
#                       autotick = TRUE,
#                       ticks = "outside",
#                       tick0 = 0,
#                       ticklen = 4,
#                       tickwidth = 1,
#                       tickcolor = toRGB("black")),
#          yaxis = list(title = "Hoehe [m]",
#                       range = c(0, h_max + 5),
#                       zeroline = TRUE,
#                       autotick = TRUE,
#                       ticks = "outside",
#                       tick0 = 0,
#                       ticklen = 4,
#                       tickwidth = 1,
#                       tickcolor = toRGB("black")));
#
# p
#
#
#
# p <- plot_ly(data = dt_sub,
#              x = ~Bhd/10,
#              y = ~M_Hoe/10,
#              type = 'scatter',
#              mode = 'markers',
#              color = ~ba_ehk,
#              symbols = c('circle', 'triangle-down'),
#              symbol = ~as.factor(Bs),
#              text = ~paste('Bhd: ', Bhd/10,
#                            '<br>M_Hoe:', M_Hoe/10,
#                            '<br>ba_ehk:', ba_ehk,
#                            '<br>Kh:', Kh,
#                            '<br>Bs:', Bs)) %>%
#   add_lines(data = dt_ehk,
#             x = ~bhd,
#             y = ~h_ehk,
#             name = 'h_ehk',
#             linetype = ~as.factor(id),
#             color = ~ba_ehk,
#             inherit = FALSE,
#             line = list(dash = 'dash'),
#             text = ~paste('ba_ehk: ', ba_ehk,
#                           '<br>Kh:', Kh,
#                           '<br>Bs:', Bs)) %>%
#   add_markers(inherit = TRUE,
#               data = dt_sub,
#               x = ~Bhd/10,
#               y = ~h_pred,
#               marker = list(
#                 line = list(
#                   color = 'rgb(0, 0, 0)',
#                   width = 1
#                 )
#               ),
#               text = ~paste('Bhd: ', Bhd/10,
#                             '<br>h_pred:', round(h_pred, 1),
#                             '<br>ba_ehk:', ba_ehk,
#                             '<br>Kh:', Kh,
#                             '<br>Bs:', Bs)) %>%
#   layout(xaxis = list(title = 'Bhd [cm]',
#                       range = c(0, bhd_max + 10),
#                       zeroline = TRUE,
#                       autotick = TRUE,
#                       ticks = "outside",
#                       tick0 = 0,
#                       ticklen = 4,
#                       tickwidth = 1,
#                       tickcolor = toRGB("black")),
#          yaxis = list(title = "Hoehe [m]",
#                       range = c(0, h_max + 5),
#                       zeroline = TRUE,
#                       autotick = TRUE,
#                       ticks = "outside",
#                       tick0 = 0,
#                       ticklen = 4,
#                       tickwidth = 1,
#                       tickcolor = toRGB("black")));
#
#
#
# dt_miss <- dt_trees[dt_trees_sub[, list(Tnr, Enr, Bnr)],
#                     on = c("Tnr", "Enr", "Bnr")][, list(n = .N,
#                                                         n_na = sum(is.na(M_Hoe)),
#                                                         n_kh0 = sum(Kh == 0),
#                                                         n_kh1 = sum(Kh == 1),
#                                                         n_kh2 = sum(Kh == 2),
#                                                         n_bs1 = sum(bs_group == 1),
#                                                         n_bs2 = sum(bs_group == 2)),
#                                                  keyby = list(Tnr, Enr)]
#
# ba_tarif <- dt_trees[, unique(ba_tarif)]
# h_mod_coef <- list();
# length(h_mod_coef) <- length(ba_tarif);
# names(h_mod_coef) <- ba_tarif;
#
# for (ba in ba_tarif) {
#   lm_h <- lm(M_Hoe ~ I(1/Bhd) + I(1/Bhd^2), data = dt_trees[ba_tarif == ba]);
#   h_mod_coef[[ba]] <- coef(lm_h);
#   names(h_mod_coef[[ba]]) <- c("b0", "b1", "b2");
# }
#
# h_curtis <- function(spec, d) {
#   b0 <- c(fi = 434.1235, bu = 382.0202, ta = 453.5538, ki = 359.7162, lae = 421.4473, dgl = 481.5531, ei = 348.3262);
#   b1 <- c(fi = -65586.6915, bu = -51800.9382, ta = -81132.5221, ki = -42967.9947, lae = -60241.2948, dgl = -81754.2523, ei = -46547.3645);
#   b2 <- c(fi = 3074967.1738, bu = 2374368.3254, ta = 4285801.5636, ki = 1763359.9972, lae = 2895409.6245, dgl = 4193121.2406, ei = 2119420.9444);
#
#   return(b0[tolower(spec)] + b1[tolower(spec)]*1/d + b2[tolower(spec)]*1/d^2)
# }
#
# h_curtis('ei', 500)
#
# dt_trees[ba_tarif == "FI" & Kh == 0,
#          plot(h_curtis(ba_tarif, Bhd), M_Hoe, xlim = c(0, 500), ylim = c(0, 500))]
#
# abline(0, 1)





