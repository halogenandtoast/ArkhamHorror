module Arkham.Homebrew.CircusExMortis.Enemies.MalformedDarkYoung (malformedDarkYoung) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Enemy (insteadOfDiscarding)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.CircusExMortis.Helpers (flipToVictoryDisplay, investigatorWithDestiny)
import Arkham.Matcher

newtype MalformedDarkYoung = MalformedDarkYoung EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | Hunter and Retaliate are printed keywords and live on the card def.
malformedDarkYoung :: EnemyCard MalformedDarkYoung
malformedDarkYoung = enemy MalformedDarkYoung Cards.malformedDarkYoung

{- | "__Prey__ - Investigator whose destiny is \"heart\" only." and "Malformed Dark Young
cannot be defeated by attacks or effects triggered by investigators whose destiny is not
\"heart.\""

The destiny is read from the campaign log here rather than through the scenario's
republished 'ScenarioModifier', because reading a modifier from inside a 'HasModifiersFor'
is the recursion trap; the log reader is safe. 'setOnlyPrey' cannot serve this, since it
takes a static matcher at construction and the destiny is only known at runtime.

The defeat ban is scoped with 'SourceUsedBy' rather than 'SourceOwnedBy' so it also covers
a non-heart investigator attacking through a card they do not own -- Silent Clearing's own
"spend 1-2 clues: __Fight__" is a location ability in this very scenario. Framing it as
"used by an investigator who is not the heart" rather than "not used by the heart" is
deliberate: a defeat from a source belonging to no investigator at all is not something
the printed text forbids.
-}
instance HasModifiersFor MalformedDarkYoung where
  getModifiersFor (MalformedDarkYoung a) = do
    heart <- investigatorWithDestiny "heart"
    modifySelf
      a
      [ ForcePrey $ OnlyPrey $ Prey heart
      , CannotBeDefeatedBy $ SourceUsedBy (NotInvestigator heart)
      ]

instance HasAbilities MalformedDarkYoung where
  getAbilities (MalformedDarkYoung a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyDefeated #when Anyone ByAny (be a)

instance RunMessage MalformedDarkYoung where
  runMessage msg e@(MalformedDarkYoung attrs) = runQueueT $ case msg of
    -- "Forced - After Malformed Dark Young is defeated: Flip it and move it to the victory
    -- display." This replaces the defeat's disposal rather than adding to it, so it hangs
    -- off the #when window with insteadOfDiscarding: the defeat still happened (and its
    -- windows still fire), but the card never reaches the encounter discard.
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      insteadOfDiscarding attrs
        $ flipToVictoryDisplay (Just iid) Stories.strikeTheHeart attrs.cardId
        $ RemoveEnemy attrs.id
      pure e
    _ -> MalformedDarkYoung <$> liftRunMessage msg attrs
