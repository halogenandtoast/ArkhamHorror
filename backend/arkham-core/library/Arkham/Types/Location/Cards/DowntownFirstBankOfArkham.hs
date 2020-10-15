{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.DowntownFirstBankOfArkham
  ( DowntownFirstBankOfArkham(..)
  , downtownFirstBankOfArkham
  )
where

import Arkham.Import

import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype DowntownFirstBankOfArkham = DowntownFirstBankOfArkham Attrs
  deriving newtype (Show, ToJSON, FromJSON)

downtownFirstBankOfArkham :: DowntownFirstBankOfArkham
downtownFirstBankOfArkham = DowntownFirstBankOfArkham
  $ baseAttrs "01130" "Downtown" 3 (PerPlayer 1) Triangle [Moon, T] [Arkham]

instance HasModifiersFor env investigator DowntownFirstBankOfArkham where
  getModifiersFor _ _ _ = pure []

ability :: Attrs -> Ability
ability attrs = (mkAbility (toSource attrs) 1 (ActionAbility 1 Nothing))
  { abilityLimit = PerGame
  }

instance (ActionRunner env investigator) => HasActions env investigator DowntownFirstBankOfArkham where
  getActions i NonFast (DowntownFirstBankOfArkham attrs@Attrs {..})
    | locationRevealed = do
      baseActions <- getActions i NonFast attrs
      unused <- getIsUnused i (ability attrs)
      canGainResources <-
        notElem CannotGainResources <$> getInvestigatorModifiers i attrs
      pure
        $ baseActions
        <> [ ActivateCardAbilityAction (getId () i) (ability attrs)
           | unused
             && canGainResources
             && atLocation i attrs
             && hasActionsRemaining i Nothing locationTraits
           ]
  getActions _ _ _ = pure []

instance (LocationRunner env) => RunMessage env DowntownFirstBankOfArkham where
  runMessage msg l@(DowntownFirstBankOfArkham attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      l <$ unshiftMessage (TakeResources iid 3 False)
    _ -> DowntownFirstBankOfArkham <$> runMessage msg attrs
