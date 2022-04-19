-- Create
CREATE TABLE Knihy (
  ISBN        text(4),
  nazev       text(255),
  vydavatel   text(255),
  rok_vydani  int,
  pocet_stran int,
  cena        int,
  zanr        text(255),
  typ         text(255)
);

-- Records
INSERT INTO Knihy VALUES ('K003', 'Stopařův průvodce po galaxii', 'MF', 1985, 214, 120, 'scifi', 'kniha');
INSERT INTO Knihy VALUES ('K045', 'Pán prstenů - Dvě věže', 'MF', 1948, 251, 240, 'fantasy', 'kniha');
INSERT INTO Knihy (ISBN, nazev, vydavatel, rok_vydani, cena, zanr, typ)
                  VALUES ('K051', 'Kedrigern a hlas pro princeznu', 'MF', 1996, 53, 'fantasy', 'kniha');
INSERT INTO Knihy VALUES ('K043', 'Hobit', 'MF', 1950, 410, 178, 'fantasy', 'kniha');
INSERT INTO Knihy VALUES ('K025', 'Barva kouzel', 'Talpress', 1989, 221, 358, 'fantasy', 'ebook');
INSERT INTO Knihy (ISBN, nazev, vydavatel, rok_vydani, cena, zanr, typ)
                  VALUES ('K026', 'Stráže! Stráže!', 'Talpress', 2000, 214, 'fantasy', 'ebook');
INSERT INTO Knihy VALUES ('K027', 'Lehké fantastično', 'Talpress', 1999, 145, 415, 'fantasy', 'ebook');

-- Queries
SELECT * FROM Knihy;  -- Select all
SELECT zanr FROM Knihy GROUP BY zanr;  -- Žánry bez opakování
SELECT nazev FROM Knihy WHERE nazev LIKE 'S%';  -- Název začínající na písmeno S
SELECT nazev, pocet_stran FROM Knihy WHERE pocet_stran > 200;  -- Více než 200 stran
SELECT nazev, cena FROM Knihy WHERE cena > 150 && cena < 250;  -- Cena od 150Kč do 250Kč
