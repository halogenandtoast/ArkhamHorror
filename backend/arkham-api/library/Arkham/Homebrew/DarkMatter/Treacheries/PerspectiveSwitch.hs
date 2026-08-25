module Arkham.Homebrew.DarkMatter.Treacheries.PerspectiveSwitch (perspectiveSwitch) where

import Arkham.Ability
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Keyword (Keyword (Hidden))
import Arkham.Matcher hiding (Discarded)
import Arkham.Matcher qualified as Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Treachery.Import.Lifted

newtype PerspectiveSwitch = PerspectiveSwitch TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

perspectiveSwitch :: TreacheryCard PerspectiveSwitch
perspectiveSwitch = treachery PerspectiveSwitch Cards.perspectiveSwitch

{- | "Peril. Hidden. / Revelation - Secretly add this card to your hand. /
Forced - After you discard a hidden card from your hand: Discard Perspective
Switch, disengage from all enemies and move to the revealed location farthest
from your current location."
-}
instance HasAbilities PerspectiveSwitch where
  getAbilities (PerspectiveSwitch a) =
    [ restricted a 1 InYourHand
        -- hidden cards live in hand as treachery entities, so their discard
        -- comes through the generic Discarded window, not DiscardedFromHand
        $ forced
        $ Matcher.Discarded #after (Just You) AnySource (basic $ CardWithKeyword Hidden)
    ]

instance RunMessage PerspectiveSwitch where
  runMessage msg t@(PerspectiveSwitch attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      addHiddenToHand iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectEach (enemyEngagedWith iid) (disengageEnemy iid)
      farthest <- select $ FarthestLocationFromInvestigator (InvestigatorWithId iid) RevealedLocation
      chooseTargetM iid farthest $ moveTo (attrs.ability 1) iid
      toDiscardBy iid attrs attrs
      pure t
    _ -> PerspectiveSwitch <$> liftRunMessage msg attrs
