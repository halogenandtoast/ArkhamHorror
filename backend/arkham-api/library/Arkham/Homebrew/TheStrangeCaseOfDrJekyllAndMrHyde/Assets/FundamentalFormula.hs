{- | Fundamental Formula always carries exactly two tokens, and which way they
face is the whole card: two clues is +2 {willpower}/{intellect}, two doom is +2
{combat}/{agility}, one of each splits it, all on top of the flat -1 to every
skill.

The tokens are real clues and doom, so "do not count as clues or doom" has to be
said in the engine twice over. 'IgnoreDoomOnThis' keeps the doom out of
'getDoomCount', which is what the agenda's threshold check reads, so the agenda
cannot advance off it; and the doom sweep an agenda advance rides along with is
refused below, since a token that is not doom cannot be removed as doom.
'IgnoreCluesOnThis' is the clue half: every clue count the engine reads off this
asset drops them, so they never reach the controller's clue total and no clue
matcher offers this card as somewhere to take a clue from.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Assets.FundamentalFormula (
  fundamentalFormula,
) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets, modifySelf)
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.SkillType
import Arkham.Token qualified as Token

newtype FundamentalFormula = FundamentalFormula AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fundamentalFormula :: AssetCard FundamentalFormula
fundamentalFormula = asset FundamentalFormula Cards.fundamentalFormula

clueCount :: AssetAttrs -> Int
clueCount a = Token.countTokens Token.Clue a.tokens

instance HasModifiersFor FundamentalFormula where
  getModifiersFor (FundamentalFormula a) = do
    modifySelf a [IgnoreCluesOnThis (clueCount a), IgnoreDoomOnThis a.doom]
    controllerGets a
      $ [SkillModifier s (-1) | s <- allSkills]
      <> [SkillModifier s (clueCount a) | s <- [#willpower, #intellect]]
      <> [SkillModifier s a.doom | s <- [#combat, #agility]]

instance HasAbilities FundamentalFormula where
  getAbilities (FundamentalFormula a) =
    scenarioI18n
      [ withI18nTooltip "fundamentalFormula.enter"
          $ restricted a 1 ControlsThis
          $ forced
          $ AssetEntersPlay #after (be a)
      , withI18nTooltip "fundamentalFormula.flip"
          $ restricted a 2 ControlsThis
          $ freeReaction
          $ oneOf [TurnBegins #when You, AssetEntersPlay #after (be a)]
      ]

instance RunMessage FundamentalFormula where
  runMessage msg a@(FundamentalFormula attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeClues (attrs.ability 1) attrs 1
      placeDoom (attrs.ability 1) attrs 1
      pure a
    -- "any number of tokens": each token may be flipped once, so the loop runs
    -- at most as many times as there are tokens on the card
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      doStep (clueCount attrs + attrs.doom) msg
      pure a
    DoStep n (UseThisAbility iid (isSource attrs -> True) 2) | n > 0 -> do
      chooseOneM iid $ scenarioI18n $ scope "fundamentalFormula" do
        countVar n $ questionLabeled "tokensToFlip"
        when (clueCount attrs > 0) $ labeled "flipToDoom" do
          flipCluesToDoom attrs 1
          doNextStep msg
        when (attrs.doom > 0) $ labeled "flipToClue" do
          flipDoomToClues attrs 1
          doNextStep msg
        labeled "doneFlipping" nothing
      pure a
    FlipClues (isTarget attrs -> True) n -> do
      pure . FundamentalFormula $ attrs & tokensL %~ Token.flipClues n
    FlipDoom (isTarget attrs -> True) n -> do
      pure . FundamentalFormula $ attrs & tokensL %~ Token.flipDoom n
    RemoveAllDoom _ target | isTarget attrs target -> pure a
    -- a token that is not a clue cannot be discarded as one; the runner turns
    -- InvestigatorDiscardAllClues into RemoveAllClues inline, so refuse both
    RemoveAllClues _ target | isTarget attrs target -> pure a
    InvestigatorDiscardAllClues _ _ -> pure a
    _ -> FundamentalFormula <$> liftRunMessage msg attrs
