{- | Act 1c, The Question of Fighting.

The 'c' deck is the investigators buying their way to a fight they can win:
every Invisible Combat card they survive is worth a clue, and so is every clue
they are willing to carry into Griffin's own location and spend.
-}
module Arkham.Homebrew.TheInvisibleMan.Acts.TheQuestionOfFighting (theQuestionOfFighting) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Homebrew.TheInvisibleMan.Traits (pattern InvisibleCombat)
import Arkham.I18n
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Matcher
import Arkham.Projection

newtype TheQuestionOfFighting = TheQuestionOfFighting ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theQuestionOfFighting :: ActCard TheQuestionOfFighting
theQuestionOfFighting = act (1, A) TheQuestionOfFighting Cards.theQuestionOfFighting Nothing

instance HasAbilities TheQuestionOfFighting where
  getAbilities = actAbilities \x ->
    [ scenarioI18n
        $ withI18nTooltip "theQuestionOfFighting.resolved"
        $ mkAbility x 1
        $ forced
        $ ResolvesStory #after You (StoryWithTrait InvisibleCombat)
    , scenarioI18n
        $ withI18nTooltip "theQuestionOfFighting.commit"
        $ restricted x 2 (youExist $ InvestigatorAt $ LocationWithEnemy griffin)
        $ FastAbility Free
    , restricted x 3 (CluesOnThis $ AtLeast $ PerPlayer 4) $ Objective $ forced AnyWindow
    ]

instance RunMessage TheQuestionOfFighting where
  runMessage msg a@(TheQuestionOfFighting attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeTokens (attrs.ability 1) attrs #clue 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      -- "Move any number of clues you control to this act."
      clues <- field InvestigatorClues iid
      scope "theQuestionOfFighting"
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
      -- Act 1d: "For the remainder of the scenario, set Griffin's fight value
      -- to 3." Griffin writes the value off this deck advance.
      advanceActDeck attrs
      pure a
    _ -> TheQuestionOfFighting <$> liftRunMessage msg attrs
