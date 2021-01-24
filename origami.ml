(* ======= Autor: Jakub Korzeniewski ======= *)
(* =============== Origami ================= *)
(* ====== Code Review: Szymon Łukasik ====== *)

(* == Testy: https://gitlab.com/MIMUW-wpf/testy-origami/-/tree/master/tests == *)

(* ================= Typy ================= *)

(* Punkt na płaszczyźnie *)
type point = float * float;;

(* Poskładana kartka: ile razy kartkę przebije szpilka wbita w danym punkcie *)
type kartka = point -> int;;


(* ============= Koniec typów ============= *)

(* ========== Funkcje pomocnicze ========== *)


(* Stała niedokładności *)
let eps = 1e-9;;

(* funkcja wyznaczająca wzór prostej Ax + By + C przechodzącej 
   przez punkty (x, y) i (a, b)
   point -> point -> (float * float * float) *)
let line ((x, y) : point) ((a, b) : point) = (y -. b, a -. x, (x *. b) -. (a *. y));;

(* funkcja sprawdzająca po której stronie prostej przechodzącej
   przez punkty (x, y) i (a, b) znajduje się punkt (px, py)
   Jeśli jest na prostej, zwraca 0;
   Jeśli jest po lewej stronie, zwraca 1;
   Jeśli jest po praweh stronie, zwraca -1 
   point -> point -> point -> int *)
let side ((x, y) : point) ((a, b) : point) ((px, py) : point) =
  let v = (b -. y) *. (px -. a) -. (py -. b) *. (a -. x) in
  if v = 0. then 0
  else if v > 0. then -1
  else 1

(* funkcja zwracająca odbicie punktu (x, y)
   względem prostej (a, b, c) (ax + by + c = 0)
   point -> (float * float * float) -> point *)
let image ((x, y) : point) (a, b, c) : point = 
  if b = 0. then ((-.x -. 2. *. (c /. a)), y)
  else
    (* przekształcamy prostą ax + by + c = 0 na y = (aa)x + (bb) *)
    let (aa, bb) = (a /. (-.b), c /. (-.b)) in
    (((1. -. aa *. aa) *. x +. 2. *. aa *. y -. 2. *. aa *. bb) /. (1. +. aa *. aa), 
     ((aa *. aa -. 1.) *. y +. 2. *. aa *. x +. 2. *. bb) /. (1. +. aa *. aa))


(* ========== Koniec funkcji pomocniczych ========== *)


(* funkcja zwracajaca kartke, reprezentującą domknięty 
    prostokąt o bokach równoległych do osi układu współrzędnych i lewym 
    dolnym rogu (x, y) a prawym górnym (a, b). 
    point -> point -> kartka *)
let prostokat ((x, y) : point) ((a, b) : point) : kartka =
  fun (px, py) ->
  if x -. px <= eps && px -. a <= eps &&
     y -. py <= eps && py -. b <= eps then 1
  else 0

(* funkcja zwracajaca kartke, reprezentującą kółko domknięte o środku 
    w punkcie (x, y) i promieinu r
    point -> float -> kartka *)
let kolko ((x, y) : point) (r : float) : kartka = 
  fun (px, py) ->  
  if (px -. x) *. (px -. x) +. (py -. y) *. (py -. y) -. (r *. r) <= eps then 1
  else 0;;

(* funkcja składająca kartkę k wzdłuż prostej przechodzącej
    przez punkty (x, y) i (p, q) (muszą to być różne punkty). Papier jest
    składany w ten sposób, że z prawej strony prostej (patrząc w kierunku
    od (x, y) do (p, q)) jest przekładany na lewą. Wynikiem funkcji jest
    złożona kartka. 
    point -> point -> kartka -> kartka *)
let zloz ((x, y) : point) ((p, q) : point) (k : kartka) : kartka=
  let (a, b, c) = line (x, y) (p, q) in  
  fun (px, py) -> 
    let pom = side (x, y) (p, q) (px, py) in
    if pom = 0 then (k (px, py))
    else if pom = 1 then
      (k (px, py)) + k (image (px, py) (a, b, c))
    else 0;;

(* funkcja której wynikiem jest złożenie kartki k kolejne wzdłuż wszystkich prostych z listy
    (point * point) list -> kartka -> kartka *)
let skladaj (lst : (point * point) list) (k : kartka) : kartka =
  List.fold_left (fun a (x, y) -> zloz x y a) k lst;;