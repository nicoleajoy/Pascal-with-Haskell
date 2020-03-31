program ParserTest;
var
    a : boolean = true;
    b : boolean = false;
    a , b : real;
    a , b : boolean;
begin
    for testVar := 10 to 20 do
    begin
        a := a * b;
        a := a * b;
    end;

    while true do
    begin
        a := a * b;
        a := a * b;
    end;

    case grade of
        1 : a := a * b;
        2 : a := a * b;
        3 : a := a * b;
        4 : a := a * b;
    end;

    writeln();
    writeln(1.3);
    writeln(true);
    writeln(false);
    writeln('Testing parameters: ', testvar);
end.