module Arkham.Homebrew.TheSymphonyOfErichZann.Enemies.NicolePage (nicolePage) where

import Arkham.Ability
import Arkham.Card.CardType (CardType (..))
import Arkham.Discard (HandDiscard (..))
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCardEdit)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.TheSymphonyOfErichZann.Helpers (instrumentInPlay, scenarioI18n)
import Arkham.Homebrew.TheSymphonyOfErichZann.Traits qualified as T
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

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

  ponytail: the same-cardtype restriction is enforced by the discard prompt
  below rather than by the ability's cost, because no Cost constructor expresses
  "3 of one chosen type". The gate still refuses the parley unless three cards
  of some one type are in hand, so it cannot be attempted unpaid.
  -}
  getAbilities (NicolePage a) =
    extend1 a
      $ restricted
        a
        1
        ( OnSameLocation
            <> exists (TreacheryWithTrait T.String <> InPlayTreachery)
            <> youExist (HandWith (LengthIs $ atLeast 3))
        )
      $ parleyAction_

instance RunMessage NicolePage where
  runMessage msg e@(NicolePage attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid $ scenarioI18n $ scope "nicolePage" do
        for_ [(AssetType, "asset"), (EventType, "event"), (SkillType, "skill")] \(ty, key) ->
          labeled key do
            chooseAndDiscardCardEdit iid (attrs.ability 1) \d ->
              d {discardFilter = CardWithType ty, discardAmount = 3}
            card <- fetchCard Stories.violinistsMuse
            readStory iid card Stories.violinistsMuse
      pure e
    _ -> NicolePage <$> liftRunMessage msg attrs
