module Arkham.Homebrew.Defs where

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Prelude
import Arkham.Homebrew.DarkMatter.Defs qualified as DarkMatter
import Arkham.Homebrew.CircusExMortis.Defs qualified as CircusExMortis

locationsMap :: Map CardCode CardDef
locationsMap = mapFromList $ map (toCardCode &&& id) $ DarkMatter.locations <> CircusExMortis.locations

enemiesMap :: Map CardCode CardDef
enemiesMap = mapFromList $ map (toCardCode &&& id) $ DarkMatter.enemies <> CircusExMortis.enemies

treacheriesMap :: Map CardCode CardDef
treacheriesMap = mapFromList $ map (toCardCode &&& id) $ DarkMatter.treacheries <> CircusExMortis.treacheries

playerTreacheriesMap :: Map CardCode CardDef
playerTreacheriesMap = mapFromList $ map (toCardCode &&& id) $ DarkMatter.playerTreacheries <> CircusExMortis.playerTreacheries

actsMap :: Map CardCode CardDef
actsMap = mapFromList $ map (toCardCode &&& id) $ DarkMatter.acts <> CircusExMortis.acts

agendasMap :: Map CardCode CardDef
agendasMap = mapFromList $ map (toCardCode &&& id) $ DarkMatter.agendas <> CircusExMortis.agendas

encounterAssetsMap :: Map CardCode CardDef
encounterAssetsMap = mapFromList $ map (toCardCode &&& id) $ DarkMatter.encounterAssets <> CircusExMortis.encounterAssets

playerSkillsMap :: Map CardCode CardDef
playerSkillsMap = mapFromList $ map (toCardCode &&& id) $ DarkMatter.playerSkills <> CircusExMortis.playerSkills

storiesMap :: Map CardCode CardDef
storiesMap = mapFromList $ map (toCardCode &&& id) $ DarkMatter.stories <> CircusExMortis.stories
