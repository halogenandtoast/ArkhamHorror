module Arkham.Investigator.Cards.UrsulaDowns (ursulaDowns, UrsulaDowns (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.Game.Helpers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Movement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Window (defaultWindows)

newtype Metadata = Metadata {moveAfterTest :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype UrsulaDowns = UrsulaDowns (InvestigatorAttrs `With` Metadata)
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

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
  runMessage msg i@(UrsulaDowns (attrs `With` metadata)) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let windows' = defaultWindows iid
      let decreaseCost = flip applyAbilityModifiers [ActionCostModifier (-1)]
      actions <- getActionsWith iid windows' decreaseCost
      handCards <- field InvestigatorHand iid
      let investigateCards = filter (elem #investigate . cdActions . toCardDef) handCards
      playableCards <- withModifiers iid (toModifiers attrs [ActionCostOf IsAnyAction (-1)]) $ do
        filterM (getIsPlayable iid (toSource attrs) UnpaidCost windows') investigateCards
      player <- getPlayer iid
      push
        $ AskPlayer
        $ chooseOne player
        $ map ((\f -> f windows' []) . AbilityLabel iid) (filter (`abilityIs` #investigate) actions)
        <> [targetLabel (toCardId item) [PayCardCost iid item windows'] | item <- playableCards]
      pure i
    ResolveChaosToken _drawnToken ElderSign iid | iid == toId attrs -> do
      pure $ UrsulaDowns $ attrs `with` Metadata True
    SkillTestEnds _ _ | moveAfterTest metadata -> do
      targets <- getAccessibleLocations (toId attrs) attrs
      player <- getPlayer (toId attrs)
      pushWhen (notNull targets)
        $ chooseOne player
        $ [ Label "Do not move to a connecting location" []
          , Label "Move to a connecting location"
              $ [chooseOne player $ targetLabels targets (only . Move . move (toSource attrs) (toId attrs))]
          ]
      pure $ UrsulaDowns $ attrs `with` Metadata False
    _ -> UrsulaDowns . (`with` metadata) <$> runMessage msg attrs
