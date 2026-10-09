module Arkham.Homebrew.TheSymphonyOfErichZann.Enemies.IsabelLaFratta (isabelLaFratta) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.TheSymphonyOfErichZann.Helpers (instrumentInPlay, scenarioI18n)
import Arkham.Homebrew.TheSymphonyOfErichZann.Traits qualified as T
import Arkham.Matcher
import Arkham.Token

newtype IsabelLaFratta = IsabelLaFratta EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

isabelLaFratta :: EnemyCard IsabelLaFratta
isabelLaFratta = enemy IsabelLaFratta Cards.isabelLaFratta

instance HasModifiersFor IsabelLaFratta where
  getModifiersFor (IsabelLaFratta a) = do
    unlocked <- instrumentInPlay T.Piano
    modifySelfWhen a (not unlocked) [CannotBeDamaged]

instance HasAbilities IsabelLaFratta where
  getAbilities (IsabelLaFratta a) =
    extend a
      -- "Place 1 of your clues/resources on Isabel La Fratta" -- you need one.
      $ [ tip "placeClue" $ restricted a 1 (unlocked <> youExist InvestigatorWithAnyClues) parleyAction_
        , tip "placeResource"
            $ restricted a 2 (unlocked <> youExist InvestigatorWithAnyResources) parleyAction_
        ]
      -- "If there is 1 clue and 1 resource on Isabel La Fratta: Parley. Flip."
      <> [tip "flip" $ restricted a 3 unlocked parleyAction_ | bribed]
   where
    -- All three are a bare Parley, so each says which it is.
    tip key = scenarioI18n $ withI18nTooltip ("isabelLaFratta." <> key)
    unlocked = OnSameLocation <> exists (TreacheryWithTrait T.Piano <> InPlayTreachery)
    bribed = countTokens Clue a.tokens >= 1 && countTokens Resource a.tokens >= 1

instance RunMessage IsabelLaFratta where
  runMessage msg e@(IsabelLaFratta attrs) = runQueueT $ case msg of
    -- "Place 1 of your clues on Isabel La Fratta."
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      moveTokens (attrs.ability 1) iid attrs Clue 1
      pure e
    -- "Place 1 of your resources on Isabel La Fratta."
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      moveTokens (attrs.ability 2) iid attrs Resource 1
      pure e
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      card <- fetchCard Stories.pianistsMuse
      readStory iid card Stories.pianistsMuse
      pure e
    _ -> IsabelLaFratta <$> liftRunMessage msg attrs
