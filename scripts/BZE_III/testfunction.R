
middle.point.to.line <- function(x1, x2, y1, y2, c.x0, c.y0, c.r0, l.b0, l.b1){
  # calculate coordiantes of the middle of thie line between 
  x_m_line = (x1 - x2)/2;
  y_m_line = (y1 - y2)/2;
  # calculate the parameters of the equation between the middle of the line and the centre of the circle
  b1_MC = slope(c.x0, c.y0, x_m_line, y_m_line);
  b0_MC = intercept(c.x0, c.y0, b1_MC);
  # calcualte the x corrdiante of the interception of the line between M and the centre of the cirle and the circle at the given radio
  X1_inter_MC = intersection_c_lx1(b0_MC, b1_MC, c.x0, c.y0, c.r0); 
  X2_inter_MC = intersection_c_lx2(b0_MC, b1_MC, c.x0, c.y0, c.r0);
  # insert the intersection x corodinate in the line function to get the respective y coordinate
  y1_inter_MC = l(b0_MC, b1_MC, X1_inter_MC); 
  y2_inter_MC = l(b0_MC, b1_MC, X2_inter_MC);
  # distance between the intersections (inter_MC_1, inter_MC_2) to M on the line 
  dist_C_inter_1_MC = distance(X1_inter_MC, y1_inter_MC, x_m_line, y_m_line);
  dist_C_inter_2_MC = distance(X2_inter_MC, y2_inter_MC, x_m_line, y_m_line); 
  # find the x and y coordinate of the intersection on the shorter side , which is the side to exlcude from the plot 
  X_inter_MC_shorter_side = ifelse(dist_C_inter_1_MC < dist_C_inter_2_MC, X1_inter_MC, X2_inter_MC); 
  Y_inter_MC_shorter_side = ifelse(dist_C_inter_1_MC < dist_C_inter_2_MC, y1_inter_MC, y2_inter_MC);
  # insert coordinates that are for sure on the smaller side of the two halves of the circle into the implicit equation: 
  Y_MC_implicit = l.b0  + l.b1 * X_inter_MC_shorter_side - Y_inter_MC_shorter_side;
  Y_implicit_status_M_line = case_when(Y_MC_implicit > 0 ~ "positive",          # "y imlicit has to be positive too for tree to be outside, 
                                       # as the result of the implicit equation that contains the 
                                       # point that is for sure in the smaller cirlce segment, has a positive impllciti equation result", 
                                       Y_MC_implicit < 0 ~ "negative",          # "y imlicit has to be negative for tree to be outside", 
                                       TRUE ~ "equal");
  return(Y_implicit_status_M_line)
}