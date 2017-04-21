module type CONTACT =
	sig
		type contact = (string * string * int * string * string);;

	(* Fonctions de création d'un contact*)

		(* Permet de créer un tuple de type string * string * int * string * string à partir des paramètres transmis *)
		val createContact : string -> string -> int -> string -> string -> (string * string * int * string * string)

		(* Permet de convertir un tuple de type string * string * int * string * string en type contact avec le bon format pour chaque élément*)
		val formatContact : string * string * int * string * string -> contact

		(* Permet de formater les noms composés ex : Jean-Luc *)
		val makeHyphen : string -> char -> string

		(* Permet de créer le prénom du contact *)
		val makeFirstName : string -> string



	(* Fonctions de récupération des éléments d'un contact *)

		(* Retourne le prénom du contact passé en paramètre *)
		val getFirstName : contact -> string

		(* Retourne le nom de famille du contact passé en paramètre *)
		val getLastName : contact -> string

		(* Retourne l'âge du contact passé en paramètre *)
		val getAge : contact -> int

		(* Retourne l'e-mail du contact passé en paramètre *)
		val getEmail : contact -> string

		(* Retourne le numéro de téléphone du contact passé en paramètre *)
		val getPhone : contact -> string



	(* Fonctions de vérification des éléments d'un contact *)

		(* Vérifie que le numéro de téléphone n'est composé que de chiffre et d'espaces *)
		val verifNumber : string -> int -> bool

		(* Vérifie si le numéro de téléphone est dans le bon format *)
		val verifPhone : string -> bool

		(* Vérifie le format de l'âge *)
		val verifAge : int -> bool

		(* Vérifie le format du mail *)
		val verifMail : string -> bool

		(* Permet de savoir si une string est contenue dans une autre *)
		val myStrStr : string -> String.t -> int

		(* Fonction permettant de comparer deux strings de manière non case-sensitive  *)
		val strCmpUnsensi : string -> string -> int

		(* Fonction strCmpUnsensi appliquée à tous les éléments d'un contact *)
		val cmpAllUnsensi : string -> int -> contact -> int

		(* Permet de déterminer si l'élément est composé ou non et d'appliquer la fonction correspondante*)
		val formatElem : string -> int -> string



	(* Fonction permettant d'afficher chaque élément d'un contact *)

		(* Affiche l'ID du contact *)
		val printID : int -> unit

		(* Affiche le prénom du contact *)
		val printFirstName : string -> unit

		(* Affiche le nom du contact *)
		val printLastName : string -> unit

		(* Affiche l'âge du contact *)
		val printAge : int -> unit

		(* Affiche l'e-mail du contact *)
		val printEmail : string -> unit

		(* Affiche le numéro de téléphone du contact *)
		val printPhone : string -> unit

		(* Appelle les fonctions ci-dessus pour afficher entièrement un contact *)
		val printAll : int -> contact -> unit



	(* Fonctions utilitaires  *)

		(* Retourne le nombre d'éléments contenue dans une liste *)
		val sizelst : 'a list -> int

		(* Permet de raccourcir une string depuis l'index X et de taille Y *)
		val strSub : string -> int -> int -> string

		(* Permet d'ajouter le bon nombre d'espace à la fin d'une string *)
		val addString : string -> int -> string
	end

module Contact : CONTACT =
	struct
		type contact = (string * string * int * string * string);;

		let	strSub s en beg =
			if String.length s < en
				then s
			else if String.length s < beg
				then s
			else
				let buff = Buffer.create 0
				in Buffer.add_string buff s ; Buffer.sub buff beg en

		let makeHyphen str chr =
			if String.contains str chr = false
				then str
			else if String.length str < String.index str chr
				then str
			else
				let partA = strSub str ((String.index str chr) + 1) 0
				and partB = strSub str (((String.length str) - String.index str chr) - 1) ((String.index str chr) + 1)
				in partA ^ (String.capitalize_ascii partB)

		let makeFirstName = function
			| hyphenName when String.contains hyphenName '-' = true -> makeHyphen hyphenName '-'
			| spaceName when String.contains spaceName ' ' = true -> makeHyphen spaceName ' '
			| aposName when String.contains aposName '\'' = true -> makeHyphen aposName '\''
			| str -> makeHyphen str ' '

		let createContact f l a e p =
			(f, l, a, e, p)

		let formatContact (f, l, a, e, p) =
			(String.capitalize_ascii (makeFirstName (String.lowercase_ascii (String.trim f))), String.uppercase_ascii (String.trim l), a, e, p)

		let	verifAge age =
			if age <= 0 || age >= 120
				then false
			else true

		let getFirstName (f, _, _, _, _) = f;;

		let getLastName (_, l, _, _, _) = l;;

		let getAge (_, _, a, _, _) = a;;

		let getEmail (_ , _, _, e, _) = e;;

		let getPhone (_, _, _, _, p) = p;;

		let sizelst l = List.fold_left (fun acc _ -> acc + 1) 0 l;;

		let addString str x =
			let buff = Buffer.create 0
			in Buffer.add_string buff str ;
			Buffer.add_string buff "                                    " ; Buffer.sub buff 0 x

		let formatElem str x =
			if x < String.length str
				then strSub str x 0
			else
				addString str x

		let printID id = print_string (formatElem (string_of_int id) 4)

		let printFirstName firstName = print_string (formatElem firstName 16)

		let printLastName lastName = print_string (formatElem lastName 16)

		let printAge age = print_string (formatElem (string_of_int age) 4)

		let printEmail email = print_string (formatElem email 32)

		let printPhone phone = print_string (formatElem phone 14) ; print_endline ""

		let printAll id contactT =
			printID id; printFirstName (getFirstName contactT) ; printLastName (getLastName contactT) ; printAge (getAge contactT) ;
			printEmail (getEmail contactT) ; printPhone (getPhone contactT)

		let myStrStr str toFind =
			let rec loop index = match index with
				| index when String.length str < String.length toFind -> -1
				| index when String.length str - index < String.length toFind -> -1
				| _ -> if String.equal toFind (String.sub str index (String.length toFind)) = true then 0 else loop (index + 1)
			in loop 0

		let strCmpUnsensi refStr toCheck =
			let lowRef = String.lowercase_ascii refStr
			and lowToCheck = String.lowercase_ascii toCheck
			in myStrStr lowToCheck lowRef

		let cmpAllUnsensi refStr id tupleToCheck =
			if strCmpUnsensi refStr (getFirstName tupleToCheck) = 0 ||
				strCmpUnsensi refStr (getLastName tupleToCheck) = 0 ||
				strCmpUnsensi refStr (string_of_int id) = 0 ||
				strCmpUnsensi refStr (string_of_int (getAge tupleToCheck)) = 0 ||
				strCmpUnsensi refStr (getEmail tupleToCheck) = 0 ||
				strCmpUnsensi refStr (getPhone tupleToCheck) = 0
			then 0
			else -1

		let rec verifNumber str idx =
			match String.get str idx with
				| '0' .. '9' when idx = ((String.length str) - 1) -> true
				| '0' .. '9' -> true && verifNumber str (idx + 1)
				| ' ' when idx = 2 || idx = 5 || idx = 8 || idx = 11 -> true && verifNumber str (idx + 1)
				| _ -> false

		let verifPhone  = function
			| str when String.get str 0 <> '0' -> false
			| str when String.length str <> 14 -> false
			| str when String.get str 2 <> ' ' && String.get str 5 <> ' ' && String.get str 8 <> ' ' && String.get str 11 <> ' ' -> false
			| str when verifNumber str 0 = false -> false
			| _ -> true

		let verifMail = function
			| str when String.length str < 5 -> false
			| str when (String.index str '@') <= 0 -> false
			| str when (String.index str '@') <> (String.rindex str '@') -> false
			| str when (String.rindex str '.' ) = ((String.length str) - 1) -> false
			| str when (String.rindex str '.') < (String.rindex str '@') -> false
			| str when (String.rindex str '.') = ((String.rindex str '@') + 1) -> false
			| _ -> true
	end
