module Arkham.Agenda.CardDefs.TheLabyrinthsOfLunacy where

import Arkham.Agenda.CardDefs.Import

awakeningEpicMultiplayer :: CardDef
awakeningEpicMultiplayer = agenda "70002" "Awakening" 1 LabyrinthsOfLunacyEpicMultiplayer

awakening :: CardDef
awakening = agenda "70003" "Awakening" 1 LabyrinthsOfLunacySingleGroup

agonyAndDespairEpicMultiplayer :: CardDef
agonyAndDespairEpicMultiplayer = agenda "70004" "Agony and Despair" 2 LabyrinthsOfLunacyEpicMultiplayer

agonyAndDespair :: CardDef
agonyAndDespair = agenda "70005" "Agony and Despair" 2 LabyrinthsOfLunacySingleGroup

theMastermind :: CardDef
theMastermind = agenda "70006" "The Mastermind" 3 TheLabyrinthsOfLunacy
