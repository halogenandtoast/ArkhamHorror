{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.TwistedUnderbrush where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
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
                                          EncounterSet.CurseOfTheRougarou
                                          3
                                          (PerPlayer 1)
                                          Moon
                                          [Diamond, Moon]
                                          [Wilderness]
                                        )
  { locationVictory = Just 1
  }

instance HasModifiersFor env TwistedUnderbrush where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env TwistedUnderbrush where
  getActions iid NonFast (TwistedUnderbrush attrs@Attrs {..})
    | locationRevealed = do
      baseActions <- getActions iid NonFast attrs
      hasActionsRemaining <- getHasActionsRemaining
        iid
        Nothing
        (setToList locationTraits)
      pure
        $ baseActions
        <> [ ActivateCardAbilityAction
               iid
               (mkAbility (LocationSource "81007") 1 (ActionAbility 1 Nothing))
           | iid `member` locationInvestigators && hasActionsRemaining
           ]
  getActions i window (TwistedUnderbrush attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env TwistedUnderbrush where
  runMessage msg l@(TwistedUnderbrush attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      l <$ unshiftMessages
        [TakeResources iid 2 False, InvestigatorAssignDamage iid source 0 1]
    _ -> TwistedUnderbrush <$> runMessage msg attrs
