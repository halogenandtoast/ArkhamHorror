{- | Adapted Formula, the story card act 1 stands up next to the act deck.

Its gathering ability is the one act 2 prints (see
"Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Acts.Gather"), aimed at the
last two cards of the arrangement instead of the first three. Fill those and the
card flips -- and the far side of this card is the Sinister Chemist, who comes
down on the investigator who flipped it.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Stories.AdaptedFormula (
  adaptedFormula,
) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.Investigator (getJustLocation)
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Acts.Gather
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers
import Arkham.Matcher
import Arkham.Story.Import.Lifted

newtype AdaptedFormula = AdaptedFormula StoryAttrs
  deriving anyclass IsStory
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

adaptedFormula :: StoryCard AdaptedFormula
adaptedFormula = story AdaptedFormula Cards.adaptedFormula

instance HasModifiersFor AdaptedFormula where
  getModifiersFor (AdaptedFormula a) = gatherDifficulty a 1

instance HasAbilities AdaptedFormula where
  getAbilities (AdaptedFormula a) =
    [ gatherAbility "adaptedFormula.gather" a 1
    , scenarioI18n
        $ withI18nTooltip "adaptedFormula.flip"
        $ restricted
          a
          2
          (exists (arrangementWith 4 $ atLeast 1) <> exists (arrangementWith 5 $ atLeast 2))
        $ freeReaction (RoundEnds #when)
    , gatherClueAbility a 3 1
    ]

instance RunMessage AdaptedFormula where
  runMessage msg s@(AdaptedFormula attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      beginGatherTest (attrs.ability 1) attrs iid
      pure s
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      -- "Flip this card and move it to your location": the enemy is this card's
      -- other face, so it keeps the card id rather than minting a new copy.
      lid <- getJustLocation iid
      removeStory attrs
      createEnemyAt_ (lookupCard Enemies.sinisterChemist (toCardId attrs)) lid
      pure s
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      askCluesToSpend "adaptedFormula" attrs iid
      pure s
    ResolveAmounts iid (getChoiceAmount "$clues" -> spent) (isTarget attrs -> True) -> do
      spendCluesForSkillValue (attrs.ability 1) iid spent
      pure s
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      placeGatheredResource (attrs.ability 1) iid
      pure s
    _ -> AdaptedFormula <$> liftRunMessage msg attrs
