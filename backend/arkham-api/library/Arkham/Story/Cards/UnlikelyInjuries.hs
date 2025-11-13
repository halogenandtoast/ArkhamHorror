module Arkham.Story.Cards.UnlikelyInjuries (unlikelyInjuries) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Placement
import Arkham.Projection
import Arkham.Scenarios.FortuneAndFolly.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype UnlikelyInjuries = UnlikelyInjuries StoryAttrs
  deriving anyclass IsStory
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unlikelyInjuries :: StoryCard UnlikelyInjuries
unlikelyInjuries = persistStory $ story UnlikelyInjuries Cards.unlikelyInjuries

instance HasAbilities UnlikelyInjuries where
  getAbilities (UnlikelyInjuries a) =
    [ playerLimit PerTurn
        $ restricted a 1 (DuringTurn You)
        $ FastAbility
        $ OrCost
          [ HorrorCost (a.ability 1) YouTarget 1
          , DamageCost (a.ability 1) YouTarget 1
          ]
    ]

instance HasModifiersFor UnlikelyInjuries where
  getModifiersFor (UnlikelyInjuries a) = do
    let
      withInjury x =
        InvestigatorWithModifier (ScenarioModifier x)
          <> not_ (InvestigatorWithModifier (ScenarioModifier "ignoreInjury"))
      checkInjury x action = selectEach (withInjury x) \iid -> do
        takenActions <- concat <$> field InvestigatorActionsTaken iid
        when (action `elem` takenActions) $ modified_ a iid [CannotTakeAction $ IsAction action]
    checkInjury "heartInjury" #move
    checkInjury "diamondInjury" #investigate
    checkInjury "clubInjury" #fight
    checkInjury "spadeInjury" #activate

instance RunMessage UnlikelyInjuries where
  runMessage msg s@(UnlikelyInjuries attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      eachInvestigator \iid -> checkGameIcons attrs iid NoMulligan 1
      pure $ UnlikelyInjuries $ attrs & placementL .~ Global
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      turnModifier iid (attrs.ability 1) iid (ScenarioModifier "ignoreInjury")
      pure s
    DiscardedCards iid _ (isTarget attrs -> True) cards -> do
      cards' <- cards & mapMaybeM \c -> (c,) <$$> toPlayingCard c
      focusCards (fst <$> cards') $ continue_ iid
      case cards' of
        ((_, pc) : _) -> do
          case pc.suit of
            Hearts -> gameModifier attrs iid (ScenarioModifier "heartInjury")
            Diamonds -> gameModifier attrs iid (ScenarioModifier "diamondInjury")
            Clubs -> gameModifier attrs iid (ScenarioModifier "clubInjury")
            Spades -> gameModifier attrs iid (ScenarioModifier "spadeInjury")
        _ -> pure ()
      pure s
    _ -> UnlikelyInjuries <$> liftRunMessage msg attrs
