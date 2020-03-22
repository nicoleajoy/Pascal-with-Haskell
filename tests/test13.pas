(* Output:
not(a and b) = true
((a or b)or a) and b) = false
(a and b) and b = false
(a and b) and b = false
Condition is true
*)

program test13;
var    
  a, b, c: boolean;

begin
  a := true;
  b := false;
  c := not(a and b);
  writeln('not(a and b) = ', c);
  c := ((a or b)or a) and b;
  writeln('((a or b)or a) and b) = ' , c);
  a := false;
  b := true;
  c := (a and b) and b;
  writeln('(a and b) and b = ' , c);
  c := (a and b) and b;
  writeln('(a and b) and b = ' , c);
  if (a or b) then
      writeln('Condition is true' )
   else
      writeln('Condition is not true');
  end.
