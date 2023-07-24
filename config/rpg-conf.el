(defun d6 (&optional n)
  "Roll N d6. Defaults to 1."
  (interactive "P")
  (setq n (or n 1)
	r (cl-loop repeat n sum (1+ (random 6))))
  (message "%sd6: %d" n r)
  r)

(defun d8 (&optional n)
  "Roll N d8. Defaults to 1."
  (interactive "P")
  (setq n (or n 1)
	r (cl-loop repeat n sum (1+ (random 8))))
  (message "%sd8: %d" n r)
  r)

(defun d10 (&optional n)
  "Roll N d10. Defaults to 1."
  (interactive "P")
  (setq n (or n 1)
	r (cl-loop repeat n sum (1+ (random 10))))
  (message "%sd10: %d" n r)
  r)

(defun d20 (&optional n)
  "Roll N d20. Defaults to 1."
  (interactive "P")
  (setq n (or n 1)
	r (cl-loop repeat n collecting (1+ (random 20))))
  (message "%sd6: %S" n r)
  r)

(defun rpg-nightling ()
  "Insert nightling stats (elves, dwarves, goblins)."
  (interactive)
  (insert (format "Damage-%d Endurance-%d Life-%d Attack-%d Hit-%d Escape-%d"
		  (d6 2) (d6) (d6) (+ 4 (d6)) (+ 5 (d6)) (+ 3 (d6)))))

(defun rpg-predator ()
  "Insert predator stats (dogs, wolves, lions)."
  (interactive)
  (insert (format "Damage-%d Endurance-%d Life-%d Attack-%d Hit-%d Escape-%d"
		  (d6 2) (d6 3) (d6 2) (+ 5 (d6)) (+ 6 (d6)) (+ 2 (d6)))))

(defun rpg-undead ()
  "Insert undead stats (zombies, wights, vampires)."
  (interactive)
  (insert (format "Damage-%d Endurance-%d Life-%d Attack-%d Hit-%d Escape-%d"
		  (d6 2) (d6 3) (d6 4) (+ 6 (d6)) (+ 7 (d6)) (d6))))

(defun rpg-mount ()
  "Insert mount stats (horses, giant weasels, giant spiders."
  (interactive)
  (insert (format "Damage-%d Endurance-%d Life-%d Attack-%d Hit-%d Escape-%d"
		  (d6 3) (d6 6) (d6 3) (+ 4 (d6)) (+ 4 (d6)) (+ 2 (d6)))))

(defun rpg-giant ()
  "Insert giants stats."
  (interactive)
  (insert (format "Damage-%d Endurance-%d Life-%d Attack-%d Hit-%d Escape-%d"
		  (d6 4) (d6 8) (d6 4) (+ 3 (d6)) (+ 8 (d6)) (d6))))

(defun rpg-dragon ()
  "Insert dragon stats."
  (interactive)
  (insert (format "Damage-%d Endurance-%d Life-%d Attack-%d Hit-%d Escape-%d"
		  (d6 6) (d6 10) (d6 5) (+ 2 (d6)) (+ 9 (d6)) (d6))))

(defun rpg-unarmed-table ()
  "Insert a table of unarmed people.
Torchbearers, men-at-arms, those kind of people."
  (interactive)
  (insert "| Name | hp | fight? | Statement |\n")
  (dotimes (n 20)
    (let ((fight (seq-random-elt '(t nil))))
    (insert "|" (seq-random-elt rpg-names)
	    " | " (number-to-string (1+ (random 4)))
	    " | " (if fight "⚔" "🕊")
	    " |" (seq-random-elt (if fight rpg-claims rpg-excuses)) " |\n"))))

(defvar rpg-names
  '("Aadhya" "Aaliyah" "Aanya" "Aarna" "Aarusha" "Abiha" "Abira"
    "Abisana" "Abishana" "Abisheya" "Ada" "Adalia" "Adelheid" "Adelia"
    "Adina" "Adira" "Adisa" "Adisha" "Adriana" "Adriane" "Adrijana" "Aela"
    "Afriela" "Agata" "Agatha" "Aicha" "Aikiko" "Aiko" "Aila" "Ainara"
    "Aischa" "Aisha" "Aissatou" "Aiyana" "Aiza" "Aji" "Ajshe" "Akksaraa"
    "Aksha" "Akshaya" "Alaa" "Alaya" "Alea" "Aleeya" "Alegria"
    "Aleksandra" "Alena" "Alessandra" "Alessia" "Alexa" "Alexandra"
    "Aleyda" "Aleyna" "Alia" "Alice" "Alicia" "Aliena" "Alienor" "Aliénor"
    "Alija" "Alina" "Aline" "Alisa" "Alisha" "Alissa" "Alissia" "Alix"
    "Aliya" "Aliyana" "Aliza" "Alizée" "Allegra" "Allizza" "Alma" "Almira"
    "Alva" "Alva-Maria" "Alya" "Alysha" "Alyssa" "Amalia" "Amalya"
    "Amanda" "Amara" "Amaris" "Amber" "Ambra" "Amea" "Amelia" "Amelie"
    "Amélie" "Amina" "Amira" "Amor" "Amora" "Amra" "Amy" "Ana"
    "Anaahithaa" "Anabell" "Anabella" "Anaëlle" "Anaïs" "Ananya"
    "Anastasia" "Anastasija" "Anastazia" "Anaya" "Andeline" "Andjela"
    "Andrea" "Anduena" "Anela" "Anesa" "Angel" "Angela" "Angelina"
    "Angeline" "Anik" "Anika" "Anila" "Anisa" "Anise" "Anisha" "Anja"
    "Ann" "Anna" "Anna-Malee" "Annabel" "Annabelle" "Annalena" "Anne"
    "Anne-Sophie" "Annica" "Annicka" "Annigna" "Annik" "Annika" "Anouk"
    "Antonet" "Antonia" "Antonina" "Anusha" "Aralyn" "Ariane" "Arianna"
    "Ariel" "Ariela" "Arina" "Arisa" "Arishmi" "Arlinda" "Arsema" "Arwana"
    "Arwen" "Arya" "Ashley" "Ashmi" "Asmin" "Astrid" "Asya" "Athena"
    "Aubrey" "Audrey" "Aurelia" "Aurélie" "Aurora" "Ava" "Avery" "Avy"
    "Aya" "Ayana" "Ayla" "Ayleen" "Aylin" "Ayse" "Azahel" "Azra" "Barfin"
    "Batoul" "Batya" "Beatrice" "Belén" "Bella" "Bente" "Beril" "Betel"
    "Betelehim" "Bleona" "Bracha" "Briana" "Bronwyn" "Bruchi" "Bruna"
    "Büsra" "Caelynn" "Caitlin" "Caja" "Callista" "Camille" "Cao" "Carice"
    "Carina" "Carla" "Carlotta" "Carolina" "Caroline" "Cassandra"
    "Castille" "Cataleya" "Caterina" "Catherine" "Céleste" "Celia"
    "Celina" "Celine" "Ceylin" "Chana" "Chanel" "Chantal" "Charielle"
    "Charleen" "Charlie" "Charlize" "Charlott" "Charlotte" "Charly"
    "Chavi" "Chaya" "Chiara" "Chiara-Maé" "Chinyere" "Chléa" "Chloe"
    "Chloé" "Chrisbely" "Christiana" "Christina" "Ciara" "Cilgia" "Claire"
    "Clara" "Claudia" "Clea" "Cleo" "Cleofe" "Clodagh" "Cloé" "Coco"
    "Colette" "Coral" "Coralie" "Cyrielle" "Daliah" "Dalila" "Dalilah"
    "Dalina" "Damiana" "Damla" "Dana" "Daniela" "Daria" ""Darija" Dean"
    "Deborah" "Déborah-Isabel" "Defne" "Delaila" "Delia" "Delina" "Derya"
    "Deshira" "Deva" "Diana" "Diara" "Diarra" "Diesa" "Dilara" "Dina"
    "Dinora" "Djurdjina" "Dominique" "Donatella" "Dora" "Dorina" "Dunja"
    "Eda" "Edessa" "Edith" "Edna" "Eduina" "Eidi" "Eileen" "Ela" "Elanur"
    "Elda" "Eldana" "Elea" "Eleanor" "Elena" "Eleni" "Elenor" "Eleonor"
    "Eleonora" "Elhana" "Eliana" "Elidiana" "Eliel" "Elif" "Elin" "Elina"
    "Eline" "Elinor" "Elisa" "Elisabeth" "Elise" "Eliska" "Eliza" "Ella"
    "Ellen" "Elliana" "Elly" "Elma" "Elodie" "Elona" "Elora" "Elsa" "Elva"
    "Elyssa" "Emelie" "Emi" "Emilia" "Emiliana" "Emilie" "Émilie"
    "Emilija" "Emily" "Emma" "Enis" "Enna" "Enrica" "Enya" "Erdina"
    "Erika" "Erin" "Erina" "Erisa" "Erna" "Erona" "Erva" "Esma"
    "Esmeralda" "Estée" "Esteline" "Estelle" "Ester" "Esther" "Eteri"
    "Euphrasie" "Eva" "Eve" "Evelin" "Eviana" "Evita" "Ewa" "Eya" "Fabia"
    "Fabienne" "Fatima" "Fatma" "Fay" "Faye" "Fe" "Fedora" "Felia"
    "Felizitas" "Fiamma" "Filipa" "Filippa" "Filomena" "Fina" "Finja"
    "Fiona" "Fjolla" "Flaminia" "Flavia" Flor del "Carmen" "Flora"
    "Florence" "Florina" "Florita" "Flurina" "Franca" "Francesca"
    "Francisca" "Franziska" "Freija" "Freya" "Freyja" "Frida" "Gabriela"
    "Gabrielle" "Gaia" "Ganiesha" "Gaon" "Gavisgaa" "Gemma" "Georgina"
    "Ghazia" "Gia" "Giada" "Gianna" "Gila" "Gioanna" "Gioia" "Giorgia"
    "Gitty" "Giulia" "Greta" "Grete" "Gwenaelle" "Gwendolin" "Gyane"
    "Hadidscha" "Hadzera" "Hana" "Hanar" "Hania" "Hanna" "Hannah" "Hanni"
    "Hédi" "Heidi" "Helen" "Helena" "Helene" "Helia" "Heloise" "Héloïse"
    "Helya" "Henna" "Henrietta" "Heran" "Hermela" "Hiba" "Hinata"
    "Hiteshree" "Hodman" "Honey" "Iara" "Ibtihal" "Ida" "Idil" "Ilaria"
    "Ileenia" "Ilenia" "Iman" "Ina" "Indira" "Ines" "Inés" "Inez" "Ira"
    "Irene" "Iria" "Irina" "Iris" "Isabel" "Isabela" "Isabell" "Isabella"
    "Isabelle" "Isra" "Iva" "Jada" "Jael" "Jaël" "Jaelle" "Jaelynn"
    "Jalina" "Jamaya" "Jana" "Jane" "Jannatul" "Jara" "Jasmijn" "Jasmin"
    "Jasmina" "Jayda" "Jeanne" "Jelisaveta" "Jemina" "Jenna" "Jennifer"
    "Jerishka" "Jessica" "Jesuela" "Jil" "Joan" "Joana" "Joanna" "Johanna"
    "Jola" "Joleen" "Jolie" "Jonna" "Joseline" "Josepha" "Josephine"
    "Joséphine" "Joudia" "Jovana" "Joy" "Judith" "Jule" "Juli" "Julia"
    "Julie" "Juliette" "Julija" "Jully" "Juna" "Juno" "Justine" "Kahina"
    "Kaja" "Kalina" "Kalista" "Kapua" "Karina" "Karla" "Karnika"
    "Karolina" "Kashfia" "Kassiopeia" "Kate" "Katerina" "Katharina" "Kaya"
    "Kayla" "Kayley" "Kayra" "Kehla" "Keira" "Keren-Happuch" "Keziah"
    "Khadra" "Khardiata" "Kiana" "Kiara" "Kim" "Kinda" "Kira" "Klara"
    "Klea" "Kostana" "Kristina" "Kristrún" "Ksenija" "Kugagini" "Kyra"
    "Ladina" "Laetitia" "Laila" "Laís" "Lakshmi" "Lana" "Lani" "Lara"
    "Laraina" "Larina" "Larissa" "Laura" "Laurelle" "Lauri" "Laurianne"
    "Lauryn" "Lavin" "Lavinia" "Laya" "Layana" "Layla" "Lea" "Léa" "Leah"
    "Leana" "Leandra" "Leanne" "Leia" "Leilani-Yara" "Lejla" "Lelia"
    "Lena" "Leni" "Lenia" "Lenie" "Lenja" "Lenka" "Lennie" "Leona" "Leoni"
    "Leonie" "Léonie" "Leonor" "Leticia" "Leya" "Leyla" "Leyre" "Lia"
    "Liana" "Liane" "Liann" "Lianne" "Liara" "Liayana" "Liba" "Lidia"
    "Lidija" "Lijana" "Lila-Marie" "Lili" "Lilia" "Lilian" "Liliane"
    "Lilijana" "Lilith" "Lilja" "Lilla" "Lilli" "Lilly" "Lilly-Rose"
    "Lilo" "Lily" "Lin" "Lina" "Linda" "Line" "Linn" "Lioba" "Liora"
    "Lisa" "Lisandra" "Liselotte" "Liv" "Liva" "Livia" "Liz" "Loa" "Loe"
    "Lokwa" "Lola" "Lorea" "Loreen" "Lorena" "Loriana" "Lorina" "Lorisa"
    "Lotta" "Louanne" "Louisa" "Louise" "Lovina" "Lua" "Luana" "Luanda"
    "Lucia" "Luciana" "Lucie" "Lucy" "Luisa" "Luise" "Lux" "Luzia" "Lya"
    "Lyna" "Lynn" "Lynna" "Maëlle" "Maelyn" "Maëlys" "Maeva" "Magali"
    "Magalie" "Magdalena" "Mahsa" "Maira" "Maisun" "Maja" "Maka" "Malaeka"
    "Malaika" "Malea" "Maléa" "Malia" "Malin" "Malkif" "Malky" "Maltina"
    "Malu" "Manar" "Manha" "Manisha" "Mara" "Maram" "Mare" "Mareen"
    "Maren" "Margarida" "Margherita" "Margo" "Margot" "Maria" "Mariangely"
    "Maribel" "Marie" "Marie-Alice" "Marietta" "Marija" "Marika" "Mariko"
    "Marina" "Marisa" "Marisol" "Marissa" "Marla" "Marlen" "Marlene"
    "Marlène" "Marlin" "Marta" "Martina" "Martje" "Mary" "Maryam" "Mascha"
    "Mathilda" "Matilda" "Matilde" "Mauadda" "Maxine" "Maya" "Mayas"
    "Mayla" "Maylin" "Mayra" "Mayumi" "Medea" "Medina" "Meena" "Mehjabeen"
    "Mehnaz" "Meila" "Melanie" "Mélanie" "Melek" "Melian" "Melike"
    "Melina" "Melisa" "Melissa" "Mélissa" "Melka" "Melyssa" "Mena" "Meret"
    "Meri" "Merry" "Meryem" "Meta" "Mia" "Mía" "Michal" "Michelle"
    "Mihaela" "Mila" "Milania" "Milena" "Milica" "Milja" "Milla" "Milou"
    "Mina" "Mingke" "Minna" "Minu" "Mira" "Miray" "Mirdie" "Miriam"
    "Mirjam" "Mirta" "Miya" "Miyu" "Moa" "Moena" "Momo" "Momoco" "Mona"
    "Morea" "Mubera" "Muriel" "Mylène" "Myriam" "N'Dea" "Nabihaumama"
    "Nadija" "Nadin" ""Nadja" Nael" "Naemi" "Naila" "Naïma" "Naina"
    "Naliya" "Nandi" "Naomi" "Nara" "Naraya" "Nardos" "Nastasija"
    "Natalia" "Natalina" "Natania" "Natascha" "Nathalie" "Nava" "Navida"
    "Navina" "Nayara" "Nea" "Neda" "Neea" "Nejla" "Nela" "Nepheli" "Nera"
    "Nerea" "Nerine" "Nesma" "Nesrine" "Neva" "Nevia" "Nevya" "Nico"
    "Nicole" "Nika" "Nikita" "Nikolija" "Nikolina" "Nina" "Nine" "Nirya"
    "Nisa" "Nisha" "Nives" "Noa" "Noé" "Noë" "Noée" "Noelia" "Noemi"
    "Noémie" "Nola" "Nora" "Nordon" "Norea" "Norin" "Norina" "Norlha"
    "Nour" "Nova" "Nóva" "Nubia" "Nura" "Nurah" "Nuray" "Nuria" "Nuriyah"
    "Nusayba" "Oceane" "Oda" "Olive" "Olivia" "Olsa" "Oluwashayo" "Ornela"
    "Ovia" "Pamela-Anna" "Paola" "Pattraporn" "Paula" "Paulina" "Pauline"
    "Penelope" "Pepa" "Perla" "Pia" "Pina" "Rabia" "Rachel" "Rahel"
    "Rahela" "Raïssa" "Raizel" "Rajana" "Rana" "Ranim" "Raphaela" "Raquel"
    "Rayan" "Rejhana" "Rejin" "Réka" "Renata" "Rhea" "Rhynisha-Anna" "Ria"
    "Riga" "Rijona" "Rina" "Rita" "Rivka" "Riya" "Roberta" "Robin" "Robyn"
    "Rohzerin" "Róisín" "Romina" "Romy" "Ronja" "Ronya" "Rosa" "Rose"
    "Rosina" "Roxane" "Royelle" "Rozen" "Rubaba" "Rubina" "Ruby" "Rufina"
    "Rukaye" "Rumi" "Rym" "Saanvika" "Sabrina" "Sadia" "Safiya" "Sahira"
    "Sahra" "Sajal" "Salma" "Salome" "Salomé" "Samantha" "Samina" "Samira"
    "Samira-Aliyah" "Samira-Mukaddes" "Samruddhi" "Sania" "Sanna" "Sara"
    "Sarah" "Sarahi" "Saraia" "Saranda" "Saray" "Sari" "Sarina" "Sasha"
    "Saskia" "Savka" "Saya" "Sayema" "Scilla" "Sejla" "Selene" "Selihom"
    "Selina" "Selma" "Semanur" "Sena" "Sephora" "Serafima" "Serafina"
    "Serafine" "Seraina" "Seraphina" "Seraphine" "Serena" "Serra"
    "Setareh" "Shan" "Shanar" "Shathviha" "Shayenna" "Shayna" "Sheindel"
    "Shireen" "Shirin" "Shiyara" "Shreshtha" "Sia" "Sidona" "Siena"
    "Sienna" "Siiri" "Sila" "Silja" "Silvanie-Alison" "Silvia" "Simea"
    "Simi" "Simona" "Sina" "Sira" "Sirajum" "Siri" "Sirija" "Sivana"
    "Smilla" "Sofia" "Sofia-Margarita" "Sofie" "Sofija" "Solea" "Soleil"
    "Solène" "Solenn" "Sonia" "Sophia" "Sophie" "Sora" "Soraya" "Sosin"
    "Sriya" "Stella" "Stina" "Su" "Subah" "Suela" "Suhaila" "Suleqa"
    "Sumire" "Summer" "Syria" "Syrina" "Tabea" "Talina" "Tamara" "Tamasha"
    "Tamina" "Tamiya" "Tara" "Tatjana" "Tayla" "Tayscha" "Tea" "Tenzin"
    "Teodora" "Tessa" "Tharusha" "Thea" "Theniya" "Tiana" "Tijana" "Tilda"
    "Timea" "Timeja" "Tina" "Tomma" "Tonia" "Tsiajara" "Tuana" "Tyra"
    "Tzi" "Ying" "Uendi" "Uma" "Urassaya" "Vailea" "Valentina" "Valentine"
    "Valeria" "Valerie" "Vanessa" "Vanja" "Varshana" "Vella" "Vera"
    "Victoria" "Viktoria" "Vinda" "Viola" "Vivianne" "Vivien" "Vivienne"
    "Wanda" "Wayane" "Wilma" "Xin" "Xingchen" "Yael" "Yaël" "Yamina"
    "Yara" "Yasmine" "Yeilin" "Yen" "My" "Yersalm" "Yesenia" "Yeva" Yi
    "Nuo" "Yildiz-Kiymet" "Yixin" "Ylvi" "Yocheved" "Yoko" "Yosan"
    "Yosmely" "Yuen" "Yuhan" "Yuna" "Yvaine" "Zahraa" "Zaina" "Zazie"
    "Zeinab" "Zelda" "Zeliha" "Zenan" "Zerya" "Zeta" "Zeyna" "Zeynep"
    "Ziporah" "Zivia" "Zoe" "Zoé" "Zoë" "Zoë-Sanaa" "Zoey" "Zohar" "Zoi"
    "Zuri" "Aadil" "Aaron" "Abdimaalik" "Abdirahman" "Abdul" "Abdullah"
    "Abi" "Abraham" "Abrar" "Abubakar" "Achmed" "Adam" "Adan" "Adesh"
    "Adhrit" "Adil" "Adiyan" "Adrian" "Adriano" "Adrien" "Adrijan"
    "Adthish" "Advay" "Advik" "Aeneas" "Afonso" "Agustín" "Ahammed"
    "Ahnaf" "Ahron" "Aiden" "Ailo" "Aimo" "Ajan" "Ajdin" "Ajish" "Akil"
    "Akilar" "Akira" "Akito" "Aksayan" "Alan" "Aldin" "Aldion" "Alec"
    "Alejandro" "Aleksa" "Aleksandar" "Aleksander" "Aleksandr" "Alem"
    "Alessandro" "Alessio" "Alex" "Alexander" "Alexandre" "Alexandru"
    "Alexey" "Alexis" "Alfred" "Ali" "Allison" "Almir" "Alois" "Altin"
    "Aly" "Amael" "Aman" "Amar" "Amaury" "Amedeo" "Ami" "Amil" "Amin"
    "Amir" "Amirhan" "Amirthesh" "Ammar" "Amogh" "Anaël" "Anakin" "Anas"
    "Anatol" "Anatole" "Anay" "Anayo" "Andi" "Andreas" "Andrej" "Andrés"
    "Andrey" "Andri" "Andrin" "Andriy" "Andy" "Aneesh" "Anes" "Angelo"
    "Anoush" "Anqi" "Antoine" "Anton" "Antonio" "António" "Anua" "Anush"
    "Arab" "Arafat" "Aramis" "Aras" "Arbion" "Arda" "Ardit" "Arham"
    "Arian" "Arianit" "Arijon" "Arin" "Aris" "Aritra" "Ariya" "Arlind"
    "Arman" "Armin" "Arnaud" "Arne" "Arno" "Aron" "Arsène" "Art" "Artemij"
    "Arthur" "Arturo" "Arvid" "Arvin" "Aryan" "Arye" "Aswad" "Atharv"
    "Attila" "Attis" "Aulon" "Aurel" "Aurelio" "Austin" "Avinash"
    "Avrohom" "Axel" "Ayan" "Ayano" "Ayham" "Ayman" "Aymar" "Aymon"
    "Azaan" "Azad" "Azad-Can" "Bailey" "Balthazar" "Barnaba" "Barnabas"
    "Basil" "Basilio" "Bátor" "Beda" "Bela" "Ben" "Benart" "Benjamin"
    "Bennet" "Benno" "Berend" "Berktan" "Bertal" "Besir" "Bilal"
    "Bilgehan" "Birk" "Bjarne" "Bleart" "Blend" "Blendi" "Bo" "Bogdan"
    "Bolaji" "Bora" "Boris" "Brady" "Brandon" "Breyling" "Brice" "Bruce"
    "Bruno" "Bryan" "Butrint" "Caleb" "Camil" "Can" "Cário" "Carl" "Carlo"
    "Carlos" "Carmelo" "Cas" "Caspar" "Cedric" "Cédric" "Célestin"
    "Celestino" "Cemil-Lee" "César" "Chaim" "Chandor" "Charles" "Chilo"
    "Chris" "Christian" "Christopher" "Christos" "Ciaran" "Cillian" "Cla"
    "Claudio" "Colin" "Collin" "Connor" "Conrad" "Constantin" "Corey"
    "Cosmo" "Cristian" "Curdin" "Custavo" "Cynphael" "Cyprian" "Cyrill"
    "Daan" "Dagemawi" "Daha" "Dalmar" "Damian" "Damián" "Damien" "Damjan"
    "Daniel" "Daniele" "Danilo" "Danny" "Dareios" "Darel" "Darian" "Dario"
    "Daris" "Darius" "Darwin" "Davi" "David" "Dávid" "Davide" "Davin"
    "Davud" "Denis" "Deniz" "Deon" "Devan" "Devin" "Diago" "Dian" "Diar"
    "Diego" "Dilom" "Dimitri" "Dino" "Dion" "Dionix" "Dior" "Dishan"
    "Diyari" "Djamal" "Djamilo" "Domenico" "Dominic" "Dominik" "Donart"
    "Dorian" "Dries" "Drisar" "Driton" "Duart" "Duarte" "Durgut" "Durim"
    "Dylan" "Ebu" "Ebubeker" "Edgar" "Edi" "Edon" "Édouard" "Edrian"
    "Edward" "Edwin" "Efehan" "Efraim" "Ehimay" "Einar" "Ekrem" "Eldi"
    "Eldian" "Elia" "Eliah" "Elias" "Elija" "Elijah" "Elio" "Eliot"
    "Elliot" "Elouan" "Élouan" "Eloy" "Elvir" "Emanuel" "Emil" "Emilio"
    "Emin" "Emir" "Emmanuel" "Endrit" "Enea" "Enes" "Engin" "Engjëll"
    "Ennio" "Enrico" "Enrique" "Ensar" "Enzo" "Erblin" "Erd" "Eren"
    "Ergin" "Eric" "Erik" "Erind" "Erion" "Eris" "Ernest-Auguste" "Erol"
    "Eron" "Ersin" "Ervin" "Erwin" "Essey" "Ethan" "Etienne" "Evan" "Ewan"
    "Eymen" "Ezio" "Fabian" "Fabiàn" "Fabio" "Fabrice" "Fadri" "Faris"
    "Faruk" "Federico" "Félicien" "Felipe" "Felix" "Ferdinand" "Fernando"
    "Filip" "Filipe" "Finlay" "Finn" "Fionn" "Firat" "Fitz-Patrick"
    "Flavio" "Flori" "Florian" "Florin" "Flurin" "Flynn" "Francesco"
    "Frederic" "Frederick" "Frederik" "Frédo" "Fridtjof" "Fritz" "Furkan"
    "Fynn" "Gabriel" "Gabriele" "Gael" "Galin" "Gaspar" "Gaspard" "Gavin"
    "Geeth" "Genc" "Georg" "Gerald" "Geronimo" "Getoar" "Gian"
    "Gian-Andri" "Gianluca" "Gianno" "Gibran" "Gibril" "Gil" "Gil-Leo"
    "Gilles" "Gion" "Giona" "Giovanni" "Giuliano" "Giuseppe" "Glen"
    "Glenn" "Gonçalo" "Gondini" "Gregor" "Gregory" "Güney" "Guilien"
    "Guillaume" "Gustav" "Gustavo" "Gusti" "Haakon" "Haci" "Hadeed"
    "Halil" "Hamad" "Hamid" "Hamza" "Hannes" "Hans" "Hari" "Haris" "Harry"
    "Hassan" "Heath" "Hektor" "Hendri" "Henri" "Henrik" "Henry" "Henus"
    "Hugo" "Hussein" "Huw" "Iago" "Ian" "Iasu" "Ibrahim" "Idan" "Ieremia"
    "Ifran" "Iheb" "Ikechi" "Ilai" "Ilarion" "Ilian" "Ilias" "Ilja"
    "Ilyes" "Ioav" "Iorek" "Isaac" "Isak" "Ishaan" "Ishak" "Isi" "Isidor"
    "Ismael" "Ismaël" "Itay" "Ivan" "Iven" "Ivo" "Jack" "Jacob" "Jacques"
    "Jaden" "Jae-Eun" "Jago" "Jahongir" "Jake" "Jakob" "Jakov" "Jakub"
    "Jamal" "Jamen" "James" "Jamie" "Jamiro" "Jan" "Janick" "Janis" "Jann"
    "Jannes" "Jannik" "Jannis" "Janos" "János" "Janosch" "Jari" "Jaron"
    "Jasha" "Jashon" "Jason" "Jasper" "Javier" "Jawhar" "Jay" "Jayden"
    "Jayme" "Jean" "Jechiel" "Jemuël" "Jens" "Jeremias" "Jeremy" "Jerlen"
    "Jeroen" "Jérôme" "Jerun" "Jhun" "Jim" "Jimmy" "Jitzchak" "Joah"
    "Joaquin" "Joel" "Joël" "Johan" "Johann" "Johannes" "Johansel" "John"
    "Johnny" "Jon" "Jona" "Jonah" "Jonas" "Jonathan" "Joona" "Jordan"
    "Jorin" "Joris" "Jose" "Josef" "Joseph-Lion" "Josh" "Joshua" "Jovan"
    "Jovin" "Jules" "Julian" "Julien" "Julius" "Jun-Tao" "Junior" "Junis"
    "Juri" "Jurij" "Justin" "Jythin" "Kaan" "Kailash" "Kaitos" "Kajeesh"
    "Kajetan" "Kardo" "Karim" "Karl" "Karl-Nikolaus" "Kasimir" "Kaspar"
    "Kassim" "Kathiravan" "Kaynaan" "Kaynan" "Keanan" "Keano" "Kejwan"
    "Kenai" "Kennedy" "Kento" "Kerim" "Kevin" "Khodor" "Kian" "Kieran"
    "Kilian" "Kimon" "Kiran" "Kiyan" "Koji" "Konrad" "Konstantin" "Kosmo"
    "Krishang" "Krzysztof" "Kuzey" "Kyan" "Kyle" "Labib" "Lakishan"
    "Lamoral" "Lanyu" "Laris" "Lars" "Larton" "Lasse" "Laurent" "Laurenz"
    "Laurin" "Lawand" "Lawrence" "Lazar" "Lean" "Leander" "Leandro"
    "Leano" "Leart" "Leas" "Leen" "Leif" "Len" "Lenart" "Lend" "Lendrit"
    "Lenert" "Lenn" "Lennard" "Lennart" "Lenno" "Lennox" "Lenny" "Leno"
    "Leo" "Leon" "León" "Léon" "Leonard" "Leonardo" "Leonel" "Leonidas"
    "Leopold" "Leopoldo" "Leron" "Levi" "Leviar" "Levin" "Levis" "Lewis"
    "Liam" "Lian" "Lias" "Liél" "Lieven" "Linard" "Lino" "Linor" "Linus"
    "Linus-Lou" "Lio" "Lion" "Lionel" "Lior" "Liun" "Livio" "Lizhang"
    "Lloyd" "Logan" "Loïc" "Lois" Long "Yang" "Lono" "Lorenz" "Lorenzo"
    "Lorian" "Lorik" "Loris" "Lou" "Louay" "Louis" "Lovis" "Lowell" "Luan"
    "Luc" "Luca" "Lucas" "Lucian" "Lucien" "Lucio" "Ludwig" "Luis" "Luís"
    "Luk" "Luka" "Lukas" "Lumen" "Lyan" "Maaran" "Maddox" "Mads" "Mael"
    "Maél" "Máel" "Mahad" "Mahir" "Mahmoud" "Mailo" "Maksim" "Maksut"
    "Malik" "Manfred" "Máni" "Manuel" "Manuele" "Maor" "Marc" "Marcel"
    "Marco" "Marek" "Marino" "Marius" "Mark" "Marko" "Markus" "Marley"
    "Marlon" "Marouane" "Marti" "Martim" "Martin" "Marvin" "Marwin"
    "Mason" "Massimo" "Matay" "Matej" "Mateja" "Mateo" "Matheo" "Mathéo"
    "Matheus" "Mathias" "Mathieu" "Mathis" "Matia" "Matija" "Matisjohu"
    "Mats" "Matteo" "Matthew" "Matthias" "Matthieu" "Matti" "Mattia"
    "Mattis" "Maurice" "Mauricio" "Maurin" "Maurizio" "Mauro" "Maurus"
    "Max" "Maxence" "Maxim" "Maxime" "Maximilian" "Maximiliano"
    "Maximilien" "Maxmilan" "Maylon" "Median" "Mehmet" "Melis" "Melvin"
    "Memet" "Memet-Deniz" "Menachem" "Meo" "Meris" "Merlin" "Mert" "Mete"
    "Methma" "Mias" "Micah" "Michael" "Michele" "Miguel" "Mihailo"
    "Mihajlo" "Mika" "Mike" "Mikias" "Mikka" "Mikko" "Milad" "Milan"
    "Milo" "Milos" "Minyou" "Mio" "Miran" "Miraxh" "Miro" "Miron"
    "Mishkin" "Mithil" "Mohamed" "Mohammed" "Moische" "Momodou"
    "Mordechai" "Moreno" "Moritz" "Morris" "Moses" "Mubaarak" "Muhamet"
    "Muhammad" "Muhammed" "Muhannad" "Muneer" "Munzur" "Mustafa" "Nadir"
    "Nahuel" "Naïm" "Nando" "Nasran" "Nathan" "Nathanael" "Natnael"
    "Nelio" "Nelson" "Nenad" "Neo" "Néo" "Nepomuk" "Nevan" "Nevin" "Nevio"
    "Nic" "Nick" "Nick-Nolan" "Niclas" "Nicolas" "Nicolás" "Niilo" "Nik"
    "Nikhil" "Niklas" "Nikola" "Nikolai" "Nikos" "Nilas" "Nils" "Nima"
    "Nimo" "Nino" "Niven" "Nnamdi" "Noah" "Noam" "Noan" "Noè" "Noel"
    "Noël" "Nurhat" "Nuri" "Nurullmubin" "Odarian" "Odin" "Ognjen"
    "Oliver" "Olufemi" "Omar" "Omer" "Ömer" "Orell" "Orlando" "Oscar"
    "Oskar" "Osman" "Otávio" "Otto" "Ousmane" "Pablo" "Pablo-Battista"
    "Paolo" "Paris" "Pascal" "Patrice" "Patrick" "Patrik" "Paul" "Pavle"
    "Pawat" "Pax" "Paxton" "Pedro" "Peppino" "Pessach" "Peven" "Phil"
    "Philemon" "Philip" "Philipp" "Phineas" "Phoenix-Rock" "Piero"
    "Pietro" "Pio" "Pjotr" "Prashanth" "Quentin" "Quinnlan" "Quirin"
    "Rafael" "Raffael" "Raffaele" "Rainer" "Rami" "Ramí" "Ran" "Raoul"
    "Raphael" "Raphaël" "Rasmus" "Raúl" "Ray" "Rayen" "Reban" "Reda"
    "Refoel" "Rejan" "Relja" "Remo" "Remy" "Rémy" "Rénas" "Rens" "Resul"
    "Rexhep" "Rey" "Riaan" "Rian" "Riccardo" "Richard" "Rico" "Ridley"
    "Riley" "Rimon" "Rinaldo" "Rio" "Rion" "Riyan" "Riza" "Roa" "Roald"
    "Robert" "Rodney-Jack" "Rodrigo" "Roman" "Romeo" "Ronan" "Rory"
    "Rouven" "Roy" "Ruben" "Rúben" "Rubino" "Rufus" "Ryan" "Saakith"
    "Saatvik" "Sabir" "Sabit" "Sacha" "Sahl" "Salaj" "Salman" "Salomon"
    "Sami" "Sami-Abdeljebar" "Sammy" "Samuel" "Samuele" "Samy" "Sandro"
    "Santiago" "Saqlain" "Saranyu" "Sascha" "Sava" "Schloime" "Schmuel"
    "Sebastian" "Sebastián" "Selim" "Semih" "Semir" "Semyon" "Senthamil"
    "Serafin" "Seraphin" "Seth" "Sevan" "Severin" "Seya" "Seymen"
    "Seymour" "Shafin" "Shahin" "Shaor" "Sharon" "Shayaan" "Shayan"
    "Sheerbaz" "Shervin" "Shian" "Shiraz" "Shlomo" "Shon" "Siddhesh"
    "Silas" "Sileye" "Silvan" "Silvio" "Simeon" "Simon" "Sirak" "Siro"
    "Sivan" "Soel" "Sol" "Solal" "Souleiman" "Sriswaralayan" "Sruli"
    "Stefan" "Stefano" "Stephan" "Steven" "Stian" "Strahinja" "Sumedh"
    "Suryansh" "Sven" "Taavi" "Taha" "Taner" "Tanerau" "Tao" "Tarik"
    "Tashi" "Tassilo" "Tayshawn" "Temuulen" "Teo" "Teris" "Thelonious"
    "Thenujan" "Theo" "Theodor" "Thiago" "Thierry" "Thies" "Thijs" "Thilo"
    "Thom" "Thomas" "Thor" "Tiago" "Tiemo" "Til" "Till" "Tilo" "Tim"
    "Timo" "Timon" "Timothée" "Timotheos" "Timothy" "Tino" "Titus" "Tjade"
    "Tjorben" "Tobias" "Tom" "Tomás" "Tomeo" "Tosco" "Tristan" "Truett"
    "Tudor" "Tugra" "Turan" "Tyson" "Uari" "Uros" "Ursin" "Usuy" "Uwais"
    "Valentin" "Valerian" "Valerio" "Vangelis" "Vasilios" "Vico" "Victor"
    "Viggo" "Vihaan" "Viktor" "Villads" "Vincent" "Vinzent" "Vinzenz"
    "Vito" "Vladimir" "Vleron" "Vo" "Vojin" "Wander" "Wanja" "William"
    "Wim" "Xavier" "Yaakov" "Yadiel" "Yair" "Yamin" "Yanhao" "Yanic"
    "Yanik" "Yanis" "Yann" "Yannick" "Yannik" "Yannis" "Yardil" "Yared"
    "Yari" "Yasin" "Yasir" "Yavuz" "Yecheskel" "Yehudo" "Yeirol" "Yekda"
    "Yellyngton" "Yiannis" "Yifan" "Yilin" "Yitzchok" "Ylli" "Yoan"
    "Yohannes" "Yonatan" "Yonathan" "Yosias" "Younes" "Yousef" "Yousif"
    "Yousuf" "Youwei" "Ysaac" "Yuma" "Yussef" "Yusuf" "Yves" "Zaim" "Zeno"
    "Zohaq" "Zuheyb" "Zvi")
  "Some names.")

(defvar rpg-excuses
  '("I carry torches."
    "I have sworn a vow of peace. No fighting!"
    "I am a member of the porters' union."
    "I carry bags. Bags of silver and gold!"
    "I have the shakes and can't fight, but I can carry your shield."
    "I'm a shield bearer and I bear your shield, if you give it to me."
    "I swore to shed no more blood."
    "I used to be an axe murderer but now I'm sorry and I won't fight no more."
    "They won't let me sing at the local temple so I'm a singing porter, now."
    "No fighting. Just carrying."
    "I was expulsed from the water bearers guild and am looking for peaceful employment."
    "I used to be a sailor but then I got captured and ended up here. I don't want to fight."
    "I have a family to feed and need the coin. No fighting, please."
    "The gods live in peace in heaven above and so should we. I won't fight."
    "I'm slow and so I got kicked out of the town guard. I just can't fight."
    "I used to be a fighter by then I got poisoned by a giant spider and nearly died."
    "My arm was eaten by a ghoul so I can't fight but I'll never leave you behind."
    "I'll carry your spears and your shields, trust me."
    "I was a porter on the Lunar Knight expedition."
    "I carried the sword of Kind Arthur."
    "I can climb, I can swim, I can run."
    "Give me a backpack and I'll carry it."
    "Back home they called me Mule. I try to live up to the name."
    "The great Rindawan taught me humility at the porters' guild."
    "I was declared unfit to be a sailor and am looking for peaceful landlubber employment."
    "I touch no one and no one touches me. I'm a rock!"
    "The great sages teach detachment. I'm pretty detached."
    "I need money for a smoke. Got something you need carried around?"
    "I am strong. I lift. Do you lift?"
    "I have run from all my problems in life and I don't intend to stop."
    "Non-violence is the way of the unarmed."
    "When the invasion comes, I'll just move with the flow."
    "The first rule of survival is to remain unnoticed."
    "The bearing of arms begets violence."
    "Having a weapon makes you more likely to use it."
    "I don't carry weapons because I've seen too many people stab friends and family."
    "The meek shall inherit the earth."
    "If you die from sickness or old age, you end up in Hel, not Valhalla. Works for me."
    "Better to live long and be poor than to fight and leave a mauled corpse."
    "I don't find and at night I don't dream. That is my blessing.")
  "Excuses for not fighting.")

(defvar rpg-claims
  '("You have my axe! Except I don't have one."
    "I'll die for you! Just give me a sword."
    "I used to cut lumber but I don't mind killing people. Give me an axe!"
    "I'm an axe murderer without an axe. Help!"
    "I'm a lover *and* a fighter!"
    "They called me names and I took their names."
    "My middle name is Death."
    "They called me a killer and I proved them right."
    "Give me arms and armour and I'll defend you to the death."
    "Give me a club and I'll crack some bones for you."
    "They called me knuckle head and I knucked their heads!"
    "Me bash! Bash bash! Kill!"
    "I lost all my friends and family and have nothing left to live for. I'll kill for money."
    "I need the money and I have a violent temper. We'll make good friends."
    "I used to be in our village militia but then the village was burned down and here I am."
    "I pawned my dagger. I'll give you back your dagger if you lend me one."
    "I was part of the nightwatch and I know how to use a cudgel."
    "Give me a weapon and I'll wield it for you."
    "I hate all life and will happily kill for you."
    "Blood for the blood god!"
    "I promised Orcus the soul of ten innocent people for bringing back my kid, and I intend to keep the bargain."
    "I want to kill my brother but need to get some experience, first. Let's go!"
    "I was trained a sword-master in distant lands but my ship sank and I was swept ashore without my blade."
    "Set sends me dreams. Dangerous dreams."
    "In my dreams I saw Thor's hammer and I used it to smash my enemies. It was wonderful."
    "Pazuzu demands a sacrifice or he'll take me oldest child. I won't let that happen."
    "Odin sent me."
    "I am here to protect you."
    "I used to work as a bodyguard for the alderman."
    "I need to spend some time away from home."
    "My dreams are full of bloodlust and I have decided to see where this takes me."
    "My parents kicked me out and I have a lot of anger in me. A lot of anger."
    "My father was a carpenter and never got anywhere. I intend to do better."
    "I'm from a family of soldiers. My mother made it rich and I intend to do the same."
    "My friends tell me that they got all their gold in the dungeon so now I intend to get some for myself."
    "I used to be a pirate but then we had a shipwreck and I'm not going back."
    "I served in the town guard and then they caught me cheating at the docks and here I am. I can fight!"
    "I used to be the bodyguard of a tax collector. Of course I can fight."
    "I used to be the bodyguard of a mine owner collector. He is no more, sadly."
    "I can use a fighting stick. I used to make way for the magistrate."
    "I have collected many debts and I'm not afraid to collect some more."
    "My boss used to say: first you explain, then you threaten, and only then do you use violence."
    "Those who will not listen must feel it. I'm here to support you when they don't listen."
    "There is no problem in the world that cannot be solved by more violence."
    "I'm here to stab people."
    "Big Boss Garo Baba kicked me out of his bath house. I know how to stab people."
    "The elves of the sea picked me up as a kid and taught me the way of the sword."
    "Uaaargh. Uuugh. Nuk nuk. Gaaah. Nuk nak nuk nuk. Graaaaa!")
  "Claims made by people who will fight.")


;; (cl-loop for x from 18 to 31 do
;;   (cl-loop for y from 50 to 75 do
;;     (insert (format "%02d%02d ocean\n" x y))))

(defun text-map-typer ()
  (interactive)
  (let ((done nil))
    (while (not done)
      (re-search-forward "[0-9][0-9][0-9][0-9] ")
      (cl-case (read-char "Draw: ")
	(?q (setq done t))
	(?c (insert "clear"))
	(?e (insert "empty"))
	(?g (insert "grass"))
	(?s (insert "swamp"))
	(?d (insert "desert"))
	(?w (insert "water"))
	(?o (insert "ocean"))
	(?f (insert "forest"))
	(?h (insert "hills"))
	(?m (insert "mountains"))
	(?v (insert "volcano")))
      (unless done
	(delete-region (point) (line-end-position))))))

;; Random Arabic names

(defun insert-random-arabic-person ()
  "Insert a person with a random Arabic name."
  (interactive)
  (let* ((gender (if (zerop (random 2)) "♂" "♀"))
         (age (case (random 3) (0 "young") (1 "middle age") (2 "old")))
         (name (save-window-excursion
                 (find-file (format "~/Documents/RPG/Names/Arabic %s Names.txt"
                                    (if (string= gender "♂") "Masculine" "Feminine")))
                 (goto-random-line)
                 (buffer-substring (line-beginning-position) (line-end-position)))))
    (setq name (car (split-string name " - "))
          name (car (split-string name ", ")))
    (insert name ", " age " " gender)))
