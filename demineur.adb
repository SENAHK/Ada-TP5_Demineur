with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_line; use Ada.Command_Line;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
-- ajouter des librairies selon vos besoins
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Calendar; use Ada.Calendar;

procedure Demineur_Skel is
   -- type tableau pour l'aire de jeu
   type T_Board is array(Integer range <>,Integer range <>) of Integer;
   -- type tableau pour les drapeaux et les cases ouvertes/fermées
   type T_Display is array(Integer range <>,Integer range <>) of Boolean;
   Gen : Generator;

-------------------------------------------------------------------------
   -- Procédure d'affichage de l'aire de jeu tenant compte des cases
   -- ouvertes, fermées et avec drapeau
   -- Paramètre <Visibilite> : cases ouvertes/fermées
   --           <Flags>      : cases avec ou sans drapeau
   --           <Board>      : aire de jeu 
   -- NE PAS MODIFIER CETTE PROCEDURE
   procedure Affichage(Visibilite, Flags : in T_Display;
                       Board : in T_Board) is
   begin
      Put("   ");
      for J in Visibilite'range(2) loop
         Put(J,4);
      end loop;
      Put_Line(Standard_Error,"");
      for I in Visibilite'range(1) loop
         Put(I,4);
         for J in Visibilite'range(2) loop
            if Flags(I,J) then
               Put(Standard_Error,"  D ");
            else
               if Visibilite(I,J) then 
                  Put(Standard_Error,Board(I,J),3); Put(" ");                 
               else
                  Put(Standard_Error,"  . ");
               end if;
            end if;
         end loop;
         Put_Line(Standard_Error,"");
      end loop;
      Put_Line(Standard_Error,"");
   end Affichage;
   
-------------------------------------------------------------------------
	-- Procédure qui place les bombes aux positions passées en argument sur la
	-- ligne de commande via les variables <Argument_Count> (de type Natural) 
	-- et <Argument> (de type tableau de String) du paquetage <Command_Line>
	procedure Poser_Bombes(Board : in out T_Board) is
		Li,Co : Integer := 0;
	begin
		for I in 1..(Argument_Count-2)/2 loop        
			Li := Integer'Value(Argument(2*I+1));
			Co := Integer'Value(Argument(2*I+2));
			-- Remplacer cette instruction pour mettre une valeur entière
			-- représentant une bombe dans la case Board(Li,Co)
			Board(Li, Co) := -1;
			-- Put_Line("Placer une bombe dans la case (" 
                  -- & Integer'Image(Li) & "," & Integer'Image(Co) & " )"
                 -- );
		end loop;
	end Poser_Bombes;

-------------------------------------------------------------------------
	-- Procédure qui place <Nb> bombes à des positions aléatoires 
	-- dans <Board>  
	procedure Poser_Bombes(Board : in out T_Board; Nb : in out Natural) is
		NbLi, NbCo : Integer := 0;
		Li, Co : Integer := 0;
		cpt : Integer := 0;
	begin
		NbLi := Board'Length(1);
		NbCo := Board'Length(2);
		
		while Nb > 0 loop
			-- Calcul de la position aléatoire suivant le nombre de ligne/col
			Li := Integer(Float(NbLi)*Random(Gen)-0.5)+1;
			Co := Integer(Float(NbCo)*Random(Gen)-0.5)+1;
			
			-- Si case n'a pas de bombe
			if Board(Li, Co) = 0 then
				Board(Li, Co) := -1;
				-- Décrémenter nb de bombe à placer
				Nb := Nb - 1;				
			end if;
			
		end loop;
		
      --Null; -- à compléter
	end Poser_Bombes;
------------------------------------------------------------------------- 
   ----------------------------------------------------  
   -- Placer ici vos autres procédures et fonctions --
   function Case_Minee(Board : in out T_Board;
							Li, Co : in Integer) return Boolean is  
	begin		
		return Board(Li, Co) = -1;
	end Case_Minee;
	
	function Case_Ouverte(Visibilite : in T_Display; Li, Co : in Integer)
							return Boolean is
	begin
		return Visibilite(Li, Co);
	end Case_Ouverte;
	
	function Case_A_Drapeau(Flags : in T_Display; Li, Co : in Integer)
							return Boolean is
	begin
		return Flags(Li, Co);
	end Case_A_Drapeau;
	
	function Calculer_Bombes(Board : in out T_Board; Li, Co : in Integer) 
														return boolean is
		Bombe : Integer := -1;
		MaxLi : Integer := Board'Length(1);
		MaxCo : Integer := Board'Length(2);
	begin
		if Co - 1 > 0 then 
		-- Case de gauche
			if Board(Li, Co - 1) = Bombe then
				Board(Li, Co) := Board(Li, Co) + 1;
			end if;
		end if;
		
		if Co + 1 <= MaxCo then
			-- Case de droite
			if Board(Li, Co + 1) = Bombe then
				Board(Li, Co) := Board(Li, Co) + 1;
			end if;
		end if;
		
		if Li - 1 > 0 then
			-- Case du haut
			if Board(Li - 1, Co) = Bombe then
				Board(Li, Co) := Board(Li, Co) + 1;
			end if;
		end if;
		
		if Li + 1 <= MaxLi then
			-- Case du bas
			if Board(Li + 1, Co) = Bombe then
				Board(Li, Co) := Board(Li, Co) + 1;
			end if;
		end if;
		
		if Co - 1 > 0 and Li - 1 < 0 then
			-- Diagonale haut & gauche
			if Board(Li - 1, Co - 1) = Bombe then
				Board(Li, Co) := Board(Li, Co) + 1;
			end if;
		end if;
		
		if Co + 1 <= MaxCo and Li + 1 <= MaxLi then
			-- Diagonale bas & droite
			if Board(Li + 1, Co + 1) = Bombe then
				Board(Li, Co) := Board(Li, Co) + 1;
			end if;
		end if;
		
		if Li - 1 > 0 and Co + 1 <= MaxCo then
			-- Diagonale haut & droite
			if Board(Li - 1, Co + 1) = Bombe then
				Board(Li, Co) := Board(Li, Co) + 1;
			end if;
		end if;
		
		if Co - 1 > 0 and Li + 1 <= MaxCo then
			-- Diagonale bas & gauche
			if Board(Li + 1, Co - 1) = Bombe then
				Board(Li, Co) := Board(Li, Co) + 1;
			end if;
		end if;	
				
		return (Board(Li, Co) = 0);
	end Calculer_Bombes;
   
   
	procedure Ouvrir_Case(Visibilite, Flags : in out T_Display; 
							Board : in out T_Board;			
							Li, Co : in Integer) is
		MaxLi : Integer := Board'Length(1);
		MaxCo : Integer := Board'Length(2);
	begin
		
		if not Case_Minee(Board, Li, Co) and not Case_Ouverte(Visibilite, Li, Co) 
			and not Case_A_Drapeau(Flags, Li, Co) then
				Visibilite(Li, Co) := True;
				if Calculer_Bombes(Board, Li, Co)  then
					if(Co + 1 <= MaxCo) then
						Ouvrir_Case(Visibilite, Flags, Board, Li, Co + 1);
					end if;
					if(Co - 1 > 0) then
						Ouvrir_Case(Visibilite, Flags, Board, Li, Co - 1);
					end if;
					if(Li + 1 <= MaxLi) then
						Ouvrir_Case(Visibilite, Flags, Board, Li + 1, Co);
					end if;
					if(Li - 1 > 0) then
						Ouvrir_Case(Visibilite, Flags, Board, Li - 1, Co);
					end if;
					if(Co - 1 > 0 and Li -1 > 0) then
						Ouvrir_Case(Visibilite, Flags, Board, Li - 1, Co - 1);
					end if;
					if(Co + 1 <= MaxCo and Li + 1 <= MaxLi) then
						Ouvrir_Case(Visibilite, Flags, Board, Li + 1, Co + 1);				
					end if;
					if(Co + 1 <= MaxCo and Li - 1 > 0) then
						Ouvrir_Case(Visibilite, Flags, Board, Li - 1, Co + 1);				
					end if;
					if(Co - 1 > 0 and Li + 1 <= MaxLi) then
						Ouvrir_Case(Visibilite, Flags, Board, Li + 1, Co - 1);
					end if;
				end if;
		end if;
	end Ouvrir_Case;
   
	
	
	procedure Placer_Drapeau(Nb_Drapeaux : in out Integer; Visibilite, Flags : in out T_Display;						
						Li, Co : in Integer) is 
	begin
			if not Case_Ouverte(Visibilite, Li, Co) then
				if Case_A_Drapeau(Flags, Li, Co) then
					Flags(Li, Co) := False;
					Nb_Drapeaux := Nb_Drapeaux + 1;
				else
					if Nb_Drapeaux > 0 then
						Flags(Li, Co) := True;
						Nb_Drapeaux := Nb_Drapeaux - 1;
					else
						Put("/!\ Maximum de drapeaux utilisés /!\");
						New_Line;
					end if;
				end if;
			else
				Put("/!\ Case déjà ouverte /!\");
				New_Line;
			end if;
			
	end Placer_Drapeau;
	
	function Partie_Gagnee(Visibilite : in T_Display; Nb_Bombes : in Integer; 
							Nb_Drapeaux : in Integer) return Boolean is
		Total_Cases : Integer := Visibilite'Length(1)*Visibilite'Length(2);
		Total_Cases_Ouvertes : Integer := 0;
	begin
	
		for I in Visibilite'Range(2) loop
			for J in Visibilite'Range(1) loop
				if Visibilite(I, J) then
					Total_Cases_Ouvertes := Total_Cases_Ouvertes + 1;
				end if;
			end loop;
		end loop;
		
		return (Nb_Drapeaux = 0) and (Total_Cases_Ouvertes = Total_Cases - Nb_Bombes);

	end Partie_Gagnee;
   ----------------------------------------------------
-------------------------------------------------------------------------

   -- Aire de jeu de dimensions passées en ligne de commande 
   Board : T_Board(1..Integer'Value(Argument(1)),1..Integer'Value(Argument(2)))
            := (others => (others => 0));
   -- Tableaux utilisés pour la visualisation des cases ouvertes, fermées
   -- et avec drapeau         
   Visibilite, Flags : T_Display(Board'Range(1),Board'Range(2)) 
                        := (others => (others => False));
   -- Position d'une case de l'aire de jeu
   Li : Integer := Board'First(1);
   Co : Integer := Board'First(2);
   -- Nombres totaux de bombes et de drapeaux           
   Nb_Bombes, Nb_Drapeaux : Natural := 0;
   -- Variables d'état du jeu pour détecter la fin de partie
   Gagne, Perdu, Abandon : Boolean := False;
   -- Chronomètre
   Start, Temps : Integer := 0;
   -- Choix de l'action utilisateur
   Choix : Natural := 0;
   


begin -- Demineur
   --------------------------------------------------------------
   -- VOUS NE DEVEZ RIEN MODIFIER POUR LE RENDU FINAL          --
   -- SAUF AUX ENDROITS INDIQUéS "à compléter" ou "à modifier" --
   --------------------------------------------------------------
   Put_Line("Dimension du plateau:" 
        & Integer'Image(Board'Length(1)) 
        & " x" 
        & Integer'Image(Board'Length(2))
       );
   Put("Nombre de bombes: ");
   if (Argument_Count = 2) then
      Get(Nb_Bombes);
      Poser_Bombes(Board,Nb_Bombes); -- bombes posées aléatoirement
   else 
      Nb_Bombes := (Argument_Count-2)/2;
      Put_Line(Integer'Image(Nb_Bombes));
      Poser_Bombes(Board); -- bombes posées selon arguments 
                           -- passés en ligne de commande
   end if;
   Nb_Drapeaux := Nb_Bombes;
   Start := Integer(Seconds(clock)); -- à modifier
   
   Affichage(Visibilite,Flags,Board);
   
   
   loop   
      Put("Action (0. Quitter / 1. Drapeau / 2. Ouvrir case): ");
      Get(Choix);
      case Choix is
         -- Abandon
         when 0 => Abandon := True;
         -- Placer ou enlever un drapeau
         when 1 => 
            Put("Position (Ligne,Colonne) = "); 
            Get(Li); 
            Get(Co);
            --------------------
            -- à compléter!!! --
			Placer_Drapeau(Nb_Drapeaux, Visibilite, Flags, Li, Co);
			Gagne := Partie_Gagnee(Visibilite, Nb_Bombes, Nb_Drapeaux);
            --------------------
         -- Ouvrir une case / perdu si elle contient une bombe
         when 2 =>  
            Put("Position (Ligne,Colonne) = ");
            Get(Li);
            Get(Co);
            --------------------
            -- à compléter!!! --
			if Case_Minee(Board, Li, Co) then
				Perdu := true;
			else
				Ouvrir_Case(Visibilite, Flags, Board, Li, Co);
				Gagne := Partie_Gagnee(Visibilite, Nb_Bombes, Nb_Drapeaux);
			end if;
            --------------------
         -- Choix non-valides   
         when others => Put("Choix indéfini!");
      end case;
      Affichage(Visibilite,Flags,Board);
      exit when (Gagne or Perdu) or Abandon;
   end loop; 
   if Abandon then
      Put_Line("Abandon!");
   elsif Gagne then
      Put_Line("Gagné!");
      Temps := Integer(Seconds(clock)) - Start; -- à modifier
      Put_Line("Votre temps:" & Integer'Image(Temps));      
   elsif Perdu then
      Put_Line("Perdu!");
   end if;
   -- Affichage du plateau de jeu avec toutes les cases ouvertes
   Put_Line("Plateau de jeu ouvert");
   Visibilite := (others => (others => True));
   Flags := (others => (others => False));
   Affichage(Visibilite,Flags,Board);
end Demineur_Skel;






