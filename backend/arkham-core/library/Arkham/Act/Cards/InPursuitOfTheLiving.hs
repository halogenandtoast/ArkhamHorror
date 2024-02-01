module Arkham.Act.Cards.InPursuitOfTheLiving (
  InPursuitOfTheLiving (..),
  inPursuitOfTheLiving,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheCircleUndone.Memento
import Arkham.Classes
import Arkham.Helpers.Investigator
import Arkham.Id
import Arkham.Matcher
import Arkham.Trait (Trait (Spectral))

newtype Metadata = Metadata {usedLocationIds :: [LocationId]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, NoThunks)

newtype InPursuitOfTheLiving = InPursuitOfTheLiving (ActAttrs `With` Metadata)
  deriving anyclass (IsAct)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

inPursuitOfTheLiving :: ActCard InPursuitOfTheLiving
inPursuitOfTheLiving = act (2, A) (InPursuitOfTheLiving . (`with` Metadata [])) Cards.inPursuitOfTheLiving Nothing

instance HasModifiersFor InPursuitOfTheLiving where
  getModifiersFor (InvestigatorTarget _) (InPursuitOfTheLiving (a `With` _)) =
    pure $ toModifiers a [CannotDiscoverCluesAt $ NotLocation $ LocationWithTrait Spectral]
  getModifiersFor _ _ = pure []

-- Group limit once per round at each location.
-- We handle this by using the usedLocationIds metadata
instance HasAbilities InPursuitOfTheLiving where
  getAbilities (InPursuitOfTheLiving (a `With` meta))
    | onSide A a =
        [ restrictedAbility
            a
            1
            ( OnLocation
                $ LocationWithoutModifier CannotBeFlipped
                <> locationNotOneOf (usedLocationIds meta)
            )
            $ FastAbility Free
        , restrictedAbility
            a
            2
            (ExtendedCardCount 4 $ VictoryDisplayCardMatch $ CardWithTitle "Unfinished Business")
            $ Objective
            $ ForcedAbility AnyWindow
        ]
  getAbilities _ = []

instance RunMessage InPursuitOfTheLiving where
  runMessage msg a@(InPursuitOfTheLiving (attrs `With` meta)) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide A attrs -> do
      pushAll [recordSetInsert MementosDiscovered [CornHuskDoll], scenarioResolution 1]
      pure a
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      lid <- getJustLocation iid
      push $ Flip iid (toSource attrs) (toTarget lid)
      pure . InPursuitOfTheLiving $ attrs `with` Metadata (lid : usedLocationIds meta)
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      push $ AdvanceAct (toId a) (toSource attrs) AdvancedWithOther
      pure a
    _ -> InPursuitOfTheLiving . (`with` meta) <$> runMessage msg attrs
