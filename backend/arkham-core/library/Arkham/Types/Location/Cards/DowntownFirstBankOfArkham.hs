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

instance HasModifiersFor env DowntownFirstBankOfArkham where
  getModifiersFor = noModifiersFor

ability :: Attrs -> Ability
ability attrs = (mkAbility (toSource attrs) 1 (ActionAbility 1 Nothing))
  { abilityLimit = PerGame
  }

instance ActionRunner env => HasActions env DowntownFirstBankOfArkham where
  getActions iid NonFast (DowntownFirstBankOfArkham attrs@Attrs {..})
    | locationRevealed = do
      baseActions <- getActions iid NonFast attrs
      unused <- getIsUnused iid (ability attrs)
      hasActionsRemaining <- getHasActionsRemaining
        iid
        Nothing
        (setToList locationTraits)
      canGainResources <-
        notElem CannotGainResources
          <$> getInvestigatorModifiers iid (toSource attrs)
      pure
        $ baseActions
        <> [ ActivateCardAbilityAction iid (ability attrs)
           | unused
             && canGainResources
             && iid
             `member` locationInvestigators
             && hasActionsRemaining
           ]
  getActions _ _ _ = pure []

instance (LocationRunner env) => RunMessage env DowntownFirstBankOfArkham where
  runMessage msg l@(DowntownFirstBankOfArkham attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      l <$ unshiftMessage (TakeResources iid 3 False)
    _ -> DowntownFirstBankOfArkham <$> runMessage msg attrs
