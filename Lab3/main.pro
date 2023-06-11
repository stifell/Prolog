% Copyright

implement main
    open core, file, stdio

domains
    type = матплата; видеокарта; процессор; оп; ssd.
    slot = ddr4; ddr3; gddr6; lga1200; lga1700; am4; pcie_3_x4; pcie_4_x4.
    assembly = assembly(type Тип, string Название, real Цена).

class facts - accessories
    комплектующее : (integer ID, string Название, type Тип, string Марка, string Дата).
    слоты : (integer ID, slot Слот).
    интерфейс : (integer ID, slot Слот).
    продукт : (integer ID, real Цена).
    сборка : (string LVL, integer ID).

class predicates
    сумма_элем : (real* List) -> real Summa.
clauses
    сумма_элем([]) = 0.
    сумма_элем([H | T]) = сумма_элем(T) + H.

class predicates
    вывод : (main::slot*) nondeterm.
    вывод : (string*) nondeterm.
    инфа_мат : (string Название_мат) -> main::slot* Компоненты determ.
    инфа_инт : (slot Название_слота) -> string* Слот nondeterm.
    сборка_компл : (string Уровень) -> assembly*.
    сборка_сумм : (string Уровень) -> real Summa.

clauses
    вывод([X | Y]) :-
        write("\t", X),
        nl,
        вывод(Y).

    инфа_мат(X) = [ NameK || слоты(N, NameK) ] :-
        комплектующее(N, X, матплата, _, _),
        !.

    инфа_инт(X) = [ NameK || комплектующее(N, NameK, _, _, _) ] :-
        интерфейс(N, X).

    сборка_компл(X) =
        [ assembly(Type, Name, Price) ||
            сборка(X, ID),
            комплектующее(ID, Name, Type, _, _),
            продукт(ID, Price)
        ].

    сборка_сумм(X) = Summa :-
        Summa =
            сумма_элем(
                [ Price ||
                    сборка(X, ID),
                    продукт(ID, Price)
                ]).

class predicates
    write_assem : (assembly* Компл_цена).
clauses
    write_assem(L) :-
        foreach assembly(Type, Name, Price) = list::getMember_nd(L) do
            write("\t", Type, ": ", Name, " - ", Price, "\n")
        end foreach.

clauses
    run() :-
        console::init(),
        file::consult("../data.txt", accessories),
        fail.

    run() :-
        X = "MSI H510M-A PRO",
        write("Составляющие мат.платы ", X, ":\n"),
        L = инфа_мат(X),
        вывод(L),
        nl,
        fail.

    run() :-
        X = gddr6,
        write("Комплектующие в наличии с ", X, ":\n"),
        L = инфа_инт(X),
        вывод(L),
        fail.

    run() :-
        X = "Средняя",
        write("Сборка уровня ", X, ":\n"),
        write_assem(сборка_компл(X)),
        write("\t*Общая сумма сборки: "),
        write(сборка_сумм(X)),
        nl,
        fail.

    run() :-
        succeed.

end implement main

goal
    console::runUtf8(main::run).
