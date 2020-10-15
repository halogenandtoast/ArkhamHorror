{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.TwistedUnderbrush where

import Arkham.Import

import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype TwistedUnderbrush = TwistedUnderbrush Attrs
  deriving newtype (Show, ToJSON, FromJSON)

twistedUnderbrush :: TwistedUnderbrush
twistedUnderbrush = TwistedUnderbrush $ (baseAttrs
                                          "81015"
                                          "Twisted Underbrush"
                                          3
                                          (PerPlayer 1)
                                          Moon
                                          [Diamond, Moon]
                                          [Wilderness]
                                        )
  { locationVictory = Just 1
  }

instance HasModifiersFor env investigator TwistedUnderbrush where
  getModifiersFor _ _ _ = pure []

instance (IsInvestigator investigator) => HasActions env investigator TwistedUnderbrush where
  getActions i NonFast (TwistedUnderbrush attrs@Attrs {..}) | locationRevealed =
    do
      baseActions <- getActions i NonFast attrs
      pure
        $ baseActions
        <> [ ActivateCardAbilityAction
               (getId () i)
               (mkAbility (LocationSource "81007") 1 (ActionAbility 1 Nothing))
           | atLocation i attrs && hasActionsRemaining i Nothing locationTraits
           ]
  getActions i window (TwistedUnderbrush attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env TwistedUnderbrush where
  runMessage msg l@(TwistedUnderbrush attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      l <$ unshiftMessages
        [TakeResources iid 2 False, InvestigatorAssignDamage iid source 0 1]
    _ -> TwistedUnderbrush <$> runMessage msg attrs
