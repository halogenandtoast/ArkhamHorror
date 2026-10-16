{- | Act 1e, The Question of Fleeing -- the escape half of
"Arkham.Homebrew.TheInvisibleMan.Acts.TheQuestionOfFighting", one clue cheaper.
-}
module Arkham.Homebrew.TheInvisibleMan.Acts.TheQuestionOfFleeing (theQuestionOfFleeing) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Homebrew.TheInvisibleMan.Traits (pattern InvisibleEscape)
import Arkham.I18n
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Matcher
import Arkham.Projection

newtype TheQuestionOfFleeing = TheQuestionOfFleeing ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theQuestionOfFleeing :: ActCard TheQuestionOfFleeing
theQuestionOfFleeing = act (1, A) TheQuestionOfFleeing Cards.theQuestionOfFleeing Nothing

instance HasAbilities TheQuestionOfFleeing where
  getAbilities = actAbilities \x ->
    [ scenarioI18n
        $ withI18nTooltip "theQuestionOfFleeing.resolved"
        $ mkAbility x 1
        $ forced
        $ ResolvesStory #after You (StoryWithTrait InvisibleEscape)
    , scenarioI18n
        $ withI18nTooltip "theQuestionOfFleeing.commit"
        $ restricted x 2 (youExist $ InvestigatorAt $ LocationWithEnemy griffin)
        $ FastAbility Free
    , restricted x 3 (CluesOnThis $ AtLeast $ PerPlayer 3) $ Objective $ forced AnyWindow
    ]

instance RunMessage TheQuestionOfFleeing where
  runMessage msg a@(TheQuestionOfFleeing attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeTokens (attrs.ability 1) attrs #clue 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      -- "Move any number of clues you control to this act."
      clues <- field InvestigatorClues iid
      scope "theQuestionOfFleeing"
        $ countVar clues
        $ chooseAmount iid "cluesToMove" "$clues" 0 clues attrs
      pure a
    ResolveAmounts iid (getChoiceAmount "$clues" -> moved) (isTarget attrs -> True) -> do
      moveTokens (attrs.ability 2) iid attrs #clue moved
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      -- Act 1f: "For the remainder of the scenario, set Griffin's evade value
      -- to 3." Griffin writes the value off this deck advance.
      advanceActDeck attrs
      pure a
    _ -> TheQuestionOfFleeing <$> liftRunMessage msg attrs
