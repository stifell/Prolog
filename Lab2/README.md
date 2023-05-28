# Лабораторная работа №2 
Данный код на Prolog представляет собой программу для проверки совместимости комплектующих и оптимальной сборки компьютера на основе заданных правил. Он использует факты о доступных комплектующих и их характеристиках, а также правила для сопоставления слотов, проверки совместимости и расчета стоимости сборки.

Программа состоит из нескольких разделов:

### 1. Объявление домена

* **type (тип):** определяет тип комплектующего, такой как материнская плата, видеокарта, процессор и т.д.
* **slot (слот):** определяет слоты и интерфейсы, к которым может подключаться комплектующее.

### 2. Объявление класса фактов

* **accessories (комплектующее):** содержит информацию о комплектующих, включая их идентификаторы, название, тип, марку и дату выпуска.
* **facts (факты):** содержит факты о суммарной стоимости сборки.

### 3. Объявление класса предикатов:

* **сборка_цена:** проверяет, что заданная комбинация комплектующих имеет совместимые слоты и интерфейсы и вычисляет суммарную стоимость сборки.
* **доступные:** выводит доступные комплектующие для заданного слота.
* **совместимость_комплектующих:** проверяет совместимость двух комплектующих на основе слотов и интерфейсов.
* **совместимость_сборки:** проверяет, можно ли собрать полноценный компьютер из заданных комплектующих и выводит информацию о сборке и ее стоимости.
* **оптимальная_сборка:** находит оптимальную сборку компьютера по заданной сумме и выводит информацию о сборке и ее стоимости.

Главная цель программы **run()** выполняет последовательное выполнение набора предопределенных действий. В файле **data.txt** содержатся факты о комплектующих, которые будут использоваться в программе.

Программа использует модули core (для основных функций) и file (для чтения данных из файла) и выводит результаты в консоль с помощью предиката **console::runUtf8(main::run).**