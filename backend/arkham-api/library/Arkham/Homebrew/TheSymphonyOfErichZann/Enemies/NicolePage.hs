module Arkham.Homebrew.TheSymphonyOfErichZann.Enemies.NicolePage (nicolePage) where

import Arkham.Ability
import Arkham.Card.CardType (CardType (..))
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.TheSymphonyOfErichZann.Helpers (instrumentInPlay, scenarioI18n)
import Arkham.Homebrew.TheSymphonyOfErichZann.Traits qualified as T
import Arkham.I18n
import Arkham.Matcher

newtype NicolePage = NicolePage EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nicolePage :: EnemyCard NicolePage
nicolePage = enemy NicolePage Cards.nicolePage

instance HasModifiersFor NicolePage where
  getModifiersFor (NicolePage a) = do
    unlocked <- instrumentInPlay T.String
    modifySelfWhen a (not unlocked) [CannotBeDamaged]

instance HasAbilities NicolePage where
  {- "Discard 3 cards of the same cardtype (asset, event, or skill) from your
  hand: Parley. Flip this card over."

  One branch per cardtype: an 'OrCost' only offers the branches that can be
  paid, so a hand of three cards of three different types cannot reach the
  parley at all. -}
  getAbilities (NicolePage a) =
    extend1 a
      $ restricted a 1 (OnSameLocation <> exists (TreacheryWithTrait T.String <> InPlayTreachery))
      $ parleyAction
      $ OrCost
        [ LabeledCost (scenarioI18n $ scope "nicolePage" $ "$" <> labelKey key)
            $ HandDiscardCost 3
            $ basic (CardWithType ty)
        | (ty, key) <- [(AssetType, "asset"), (EventType, "event"), (SkillType, "skill")]
        ]

instance RunMessage NicolePage where
  runMessage msg e@(NicolePage attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      card <- fetchCard Stories.violinistsMuse
      readStory iid card Stories.violinistsMuse
      pure e
    _ -> NicolePage <$> liftRunMessage msg attrs
