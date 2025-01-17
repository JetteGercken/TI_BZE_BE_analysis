USE [bwineu]
GO
/****** Object:  UserDefinedFunction [ti].[BHD_AbhSD]    Script Date: 08.08.2022 07:45:26 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

ALTER FUNCTION [ti].[BHD_AbhSD] (
        @M_Bhd As smallint			-- gemessener Brusthöhendurchmesser [mm] 
		 ,@M_hBhd as smallint		    -- Meßhöhe [cm] des gemessenen Brusthöhendurchmessers
        ,@Ba As smallint				-- (Einzel-)Baumart als Zahl
        ,@Bl As smallint)				-- Nummer des Bundeslandes
		 RETURNS smallint				-- auf 130 cm Meßhöhe umgerechneter Brusthöhendurchmesser
--ALTER FUNCTION [ti].[BHD_AbhSD] (
--       @M_Bhd As smallint			-- gemessener Brusthöhendurchmesser [mm] 
--		 ,@M_hBhd as smallint		-- Meßhöhe [cm] des gemessenen Brusthöhendurchmessers
--       , @Ba As smallint				-- (Einzel-)Baumart als Zahl
--        ,@Bl As smallint)				-- Nummer des Bundeslandes
--		 RETURNS smallint				-- auf 130 cm Meßhöhe umgerechneter Brusthöhendurchmesser
AS
	BEGIN

		---- ==============================================================
		----
		----	Inhalt:	ermittelt Brusthöhendurchmesser bei abweichender Meßhöhe nach Abholzigkeitsfunktion lt. Stefan Dahm -
		----             Bhd = [M_bhd]+2*([M_hBhd]-130)/[Tangenz]   wenn M_h_Bhd<>130, Tangenz ist abhängig von Baumart und Region
		----
		----		besser als [bwi].[ti].[BHD_RegHP]
		----            Regressionsgleichung bereitgestellt von Heino Polley, verwendet in Datenerfassungssoftware 2002, 2008 und 2012
		----            BHD130 [cm] = 1.0 * [M_Bhd] * (1.0 + (0.0011 * (M_hBhd - 130)))   Runden!
		----
		----	Übergaben:	@M_Bhd As smallint - gemessener Brusthöhendurchmesser [mm], u.U. in abweichender Meßhöhe von 130cm Höhe
		----				@M_hBhd As smallint - Meßhöhe [cm] des gemessenen Brusthöhendurchmessers
		----				@Ba as smallint - Einzelbaumart (vgl. x_ba.Icode)
		----				@Bl as smallint - Bundeslandnummer (vgl. x_bl.Icode)
		----					
		----	Rückgaben:  @BHD_AbhSD - Brusthöhendurchmesser in (wirklicher) Höhe 130 cm [cm]) as smallint 	
		----
		----
		----	Abhängigkeiten:	Schlüsseltabellen x_ba.Ba_BWI1, x_bl.Region, [k_Tangenz (Ba_Bwi1, Region)].Ba_BWI1+Region
		----                    bwi.xyk1.k_tangenz entspricht hier in Datenerfassungssoftware code_bwineu.dbo.k3_abholzig
		---- 
		---- 
		----	Autor:			P. Hennig
		----	
		----	Erstellungsdatum:	10.05.2021   gekupfert und angepasst aus bwi.talle.BHD_AbhSD vom 07.02.2014
		----
		----	Version: 1.1.0.[fortlaufende Nummer]
		---- 
		----	Veränderungsprotokoll:
		----		08.08.2022 - Kommentar für Rückgabewert berichtigt (statt Grenzkreis, nun umgerechneter BHD)
		----
		----				
		---- ==============================================================
				

			-- Deklaration der Rückgabevariable	
			DECLARE @BHD_AbhSD As smallint
			----DECLARE @rBHD_AbhSD As real

			DECLARE @Ba_BWI1 As nvarchar(3) = Null	-- Baumartengruppe der BWI1
			DECLARE @Region As smallint = NULL		-- Region ={1,2,3} zu der Land gehört
			DECLARE @Tangenz As real = NULL		    -- baumarten- und regionsabhängige Konstante

			
			DECLARE @test As smallint = 2			-- 0=ohne Testausschriften bis 3=viele Testausschriften
			
			/* Beginn der Deklaration der Inputvariablen für Testzwecke */
			--DECLARE @M_Bhd As smallint = 282			-- erlaubt Werte
			--DECLARE @M_hBhd As smallint = 120			-- erlaubt Werte
			--DECLARE @Ba As smallint = 100			-- 906,   'BU'
			--DECLARE @Bl As smallint = 12			-- NI
			/* Ende der Deklaration der Inputvariablen für Testzwecke */

			SET @BHD_AbhSD = NULL		-- Standard-Rückgabewert falls Funktion nicht erfolgreich
            if (   IsNull(@M_Bhd,-1)  < 0 
			    or IsNull(@M_hBhd,-1) <0 
			    or IsNull(@Bl,-1)<1 or IsNull(@Bl,-1)>16 
				or IsNull(@Ba,-1)=-1) Goto ende		-- Parameterfehler
 
 			-- Heraussuchen der Baumartengruppe der BWI 1 (1987); in bwi-Datenbank aus bwi.xyk.x_ba
			SELECT @Ba_BWI1=code_bwineu.dbo.x3_ba.Ba_BWI1 
			FROM code_bwineu.dbo.x3_ba 
			WHERE code_bwineu.dbo.x3_ba.ICode=@Ba		  
			--if @test>1 Select @Ba As [@Ba], @Ba_BWI1 As [@Ba_BWI1]
			if IsNull(@Ba_BWI1,'')='' Goto ende		-- Parameterfehler 

			-- Heraussuchen der Region = (1,2,3); in bwi-Datenbank aus bwi.xyk.x_bl
			SELECT @Region=code_bwineu.dbo.x3_Bl.Region 
			FROM code_bwineu.dbo.x3_bl 
			WHERE code_bwineu.dbo.x3_bl.ICode=@Bl		  
			--if @test>1 Select @Bl As [@Bl], @Region As [@Region] 
			if IsNull(@Region,-1)=-1 Goto ende		-- Parameterfehler 

		   -- Heraussuchen der Konstante Tangenz; in bwi-Datenbank aus bwi.xyk1.[k_tangenz (Ba_Bwi1, Region)]
			SELECT @Tangenz=k_tangenz.Tangenz 
			FROM code_bwineu.dbo.[k3_abholzig] as k_tangenz
			WHERE Ba_BWI1=@Ba_BWI1 And Region=@Region		  
			--if @test>1 Select @Ba_BWI1 As [@Ba_BWI1], @Region As [@Region], @Tangenz As [@Tangenz] 
			if IsNull(@Region,-1)=-1 Goto ende		-- Parameterfehler 
				
           
			SET @BHD_AbhSD = CASE WHEN @M_hBHD=130 THEN @M_bhd ELSE Round(CAST(@M_bhd as real) + 2.0 * CAST((@M_hBhd-130) as real)/@Tangenz,0) END
ende:
			--if @test>1 Select 'Variablen' As Bemerkung,  @M_Bhd As '@M_Bhd', @M_hBhd As '@M_hBhd',
			--                                             @Ba As [@Ba], @Ba_BWI1 As [@Ba_BWI1], 
			--											 @Bl As [@Bl], @Region As [@Region],
			--											 @BHD_AbhSD As '@BHD_AbhSD'
		    --if @test>0 Select 'Return @BHD_AbhSD' As Bemerkung, @BHD_AbhSD As '@BHD_AbhSD'
			RETURN @BHD_AbhSD	

	END  

	
---- ==============================================================
---- Test-Code zum Funktionsaufruf:
/*
													
		select bwineu.ti.BHD_AbhSD(282,130, 100, 5);		--> 282
		select bwineu.ti.BHD_AbhSD(282,140, 100, 5);		--> 282
		select bwineu.ti.BHD_AbhSD(282,150, 100, 5);        --> 283
		select bwineu.ti.BHD_AbhSD(282,160, 100, 5);        --> 283
        select bwineu.ti.BHD_AbhSD(282,170, 100, 5);		--> 284
		select bwineu.ti.BHD_AbhSD(282,110, 100, 5);		--> 281	
		
		select bwineu.ti.BHD_AbhSD(885,150, 100, 5);		--> 886
		

		--select bwineu.ti.BHD_AbhSD(451,150);		--> 461 statt 463
		--select bwineu.ti.BHD_AbhSD(282,120);		--> 297 statt 280
		--select bwineu.ti.BHD_AbhSD(343,140);		--> 347 statt 345 
		--select bwineu.ti.BHD_AbhSD(254,160);		--> 262 statt 260 
		--select bwineu.ti.BHD_AbhSD(297,120);		--> 294 o.k.
		--select bwineu.ti.BHD_AbhSD(586,200);		--> 631 statt 636
		--select bwineu.ti.BHD_AbhSD(Null,-1);	--> Null (wegen fehlendem Bhd)
		select bwineu.ti.BHD_AbhSD(-5,-1, NULL, NULL);		--> Null (wegen unzulässigem Bhd, negativ)
		select bwineu.ti.BHD_AbhSD(200,-1, -1 , NULL);		--> Null (wegen unzulässigem Bhd, negativ)
		select bwineu.ti.BHD_AbhSD(-5,130, -1, -1 );		--> Null (wegen unzulässigem Bhd, negativ)
*/		
---- ==============================================================
