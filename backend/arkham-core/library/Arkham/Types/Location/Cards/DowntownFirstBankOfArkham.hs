{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.DowntownFirstBankOfArkham where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Trait
import Arkham.Types.Window
import ClassyPrelude

newtype DowntownFirstBankOfArkham = DowntownFirstBankOfArkham Attrs
  deriving newtype (Show, ToJSON, FromJSON)

downtownFirstBankOfArkham :: DowntownFirstBankOfArkham
downtownFirstBankOfArkham = DowntownFirstBankOfArkham
  $ baseAttrs "01130" "Downtown" 3 (PerPlayer 1) Triangle [Moon, T] [Arkham]

instance HasModifiersFor env investigator DowntownFirstBankOfArkham where
  getModifiersFor _ _ _ = pure []

instance (ActionRunner env investigator) => HasActions env investigator DowntownFirstBankOfArkham where
  getActions i NonFast (DowntownFirstBankOfArkham attrs@Attrs {..}) = do
    baseActions <- getActions i NonFast attrs
    usedAbilities <- map unUsedAbility <$> asks (getList ())
    let
      ability = (mkAbility (LocationSource "01130") 1 (ActionAbility 1 Nothing)
                )
        { abilityLimit = PerGame
        }
    pure
      $ baseActions
      <> [ ActivateCardAbilityAction (getId () i) ability
         | (getId () i, ability)
           `notElem` usedAbilities
           && locationRevealed
           && getId () i
           `elem` locationInvestigators
           && hasActionsRemaining i Nothing locationTraits
         ]
  getActions _ _ _ = pure []

instance (LocationRunner env) => RunMessage env DowntownFirstBankOfArkham where
  runMessage msg l@(DowntownFirstBankOfArkham attrs@Attrs {..}) = case msg of
    UseCardAbility iid (LocationSource lid) _ 1 | lid == locationId ->
      l <$ unshiftMessage (TakeResources iid 3 False)
    _ -> DowntownFirstBankOfArkham <$> runMessage msg attrs
