module Arkham.Investigator.Cards.UrsulaDowns (ursulaDowns, UrsulaDowns (..)) where

import Arkham.Ability
import Arkham.Game.Helpers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Movement

newtype Metadata = Metadata {moveAfterTest :: Bool}
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

newtype UrsulaDowns = UrsulaDowns (InvestigatorAttrs `With` Metadata)
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

ursulaDowns :: InvestigatorCard UrsulaDowns
ursulaDowns =
  investigator (UrsulaDowns . (`with` Metadata False)) Cards.ursulaDowns
    $ Stats {health = 7, sanity = 7, willpower = 3, intellect = 4, combat = 1, agility = 4}

instance HasAbilities UrsulaDowns where
  getAbilities (UrsulaDowns (attrs `With` _)) =
    [ playerLimit PerRound
        $ restrictedAbility attrs 1 Self
        $ freeReaction (Moves #after You AnySource Anywhere Anywhere)
    ]

instance HasChaosTokenValue UrsulaDowns where
  getChaosTokenValue iid ElderSign (UrsulaDowns (attrs `With` _)) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage UrsulaDowns where
  runMessage msg i@(UrsulaDowns (attrs `With` metadata)) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      performActionAction iid attrs #investigate
      pure i
    ResolveChaosToken _drawnToken ElderSign iid | iid == toId attrs -> do
      pure $ UrsulaDowns $ attrs `with` Metadata True
    SkillTestEnds _ _ | moveAfterTest metadata -> do
      targets <- getAccessibleLocations (toId attrs) attrs
      player <- getPlayer (toId attrs)
      when (notNull targets) do
        chooseOne
          attrs.id
          [ Label "Do not move to a connecting location" []
          , Label "Move to a connecting location"
              $ [Msg.chooseOne player $ targetLabels targets (only . Move . move attrs attrs)]
          ]
      pure $ UrsulaDowns $ attrs `with` Metadata False
    _ -> UrsulaDowns . (`with` metadata) <$> liftRunMessage msg attrs
