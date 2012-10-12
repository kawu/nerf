-- | Handling the NELexicon dictionary.

module NLP.Nerf.Dict.NELexicon
( parseNELexicon
, readNELexicon
) where

import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import NLP.Nerf.Dict.Base

-- | Parse the NELexicon into a list of entries.
parseNELexicon :: L.Text -> [Entry]
parseNELexicon = map parseLine . L.lines

parseLine :: L.Text -> Entry
parseLine line = case L.break (==';') line of
    (_type, _form) -> Entry (L.toStrict $ L.tail _form) (L.toStrict _type)
    _   -> error $ "parseLine: invalid line \"" ++ L.unpack line ++ "\""

-- | Read the dictionary from the file.
readNELexicon :: FilePath -> IO [Entry]
readNELexicon = fmap parseNELexicon . L.readFile

-- Wczytujemy kilka zasobów słownikowych a następnie dzielimy je na dwie
-- kategorie: te, które chcemy używać samodzielnie oraz te które chcemy
-- wykorzystywać w połączeniu z PoliMorfem.
--
-- 1. W tym przypadku zadanie jest w miarę proste -- należy połączyć
-- zbiór słowników [M.Map Form (S.Set NeType)] w jeden słownik NeDict
-- (M.Map Form (S.Set NeType)), a następnie zapisać go w postaci
-- binarnej.
--
-- 2. Z PoliMorfem jest trochę trudniej.
--   * Najpierw wczytujemy wszystkie słowniki z tej kategorii i łączymy
--     je w jeden zasób używając funkcji merge i diff.
--   * Wczytujemy polimorfa i zamieniamy go na BaseMap.
--   * Używamy funkcji 'merge' z pakietu polimorf.
--   * Zamieniamy słownik na DAWG,
--   * Zapisujemy go w postaci binarnej.
