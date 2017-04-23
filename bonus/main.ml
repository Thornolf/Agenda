open Contact;;
open Agenda;;

let a = []

  let ex =  ("Albert", "Pascal", 43, "pascal@orange.fr", "09 98 77 78 54")
  let exi =  ("jason", "bourne", 24, "Jason@Bourne.eu", "06 76 43 12 23")
  let ex2 =  ("jean-luc", "untresgrandnompouressayer", 56, "jl.treslongnom@epitech.eu", "06 56 43 23 88")

    let add = Agenda.addContact a ex
    let addi = Agenda.addContact add exi
    let addi = Agenda.addContact addi ex2

	let b = []

	let exE1 = ("pitch agenda", "12/12/17", "23:23", 20, addi)
	let exE2 = ("rendez-vous chez le médecin", "09/05/18", "10:20", 60, add)

	let eventList = Agenda.addEvent b exE1
	let eventList = Agenda.addEvent eventList exE2

  let _  =
	print_endline "";;
	print_endline "******************** CONTACT **********************";;
	print_endline "Affichage de tous les contacts : ";;
	Agenda.printContacts addi All "";
	print_endline "";;
	print_endline "Suppression d'un contact : ";;
	let addi = Agenda.removeContact addi 2;;
	Agenda.printContacts addi All "";;
	print_endline "";;
	print_endline "Remplacement du contact 1 : ";;
	let replContact = ("un mec", "random", 10, "mec.random@onsaitpas.eu", "00 11 22 33 44");;
	let addi = Agenda.replaceContact addi 1 replContact;;
	Agenda.printContacts addi LastName "ran";;
	print_endline "";;
	print_endline "Affichage de l'ID du contact ayant dans son prénom la string \"bert\"";;
	print_int (Agenda.getContactId addi FirstName "bert");;
	print_string " ==> ";;
	Agenda.printContacts addi FirstName "bert";;

	print_endline "";;
	print_endline "";;
	print_endline "Affichage de tous les events : ";;
	Agenda.printEvents eventList AllEvent "";
