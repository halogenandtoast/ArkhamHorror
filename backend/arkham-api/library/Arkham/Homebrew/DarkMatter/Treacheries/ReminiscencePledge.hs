module Arkham.Homebrew.DarkMatter.Treacheries.ReminiscencePledge (reminiscencePledge) where

import Arkham.Ability
import Arkham.Enemy.Types (Field (EnemyDamage))
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection
import Arkham.Treachery.Import.Lifted hiding (InvestigatorEliminated)

newtype ReminiscencePledge = ReminiscencePledge TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

reminiscencePledge :: TreacheryCard ReminiscencePledge
reminiscencePledge = treachery ReminiscencePledge Cards.reminiscencePledge

instance HasAbilities ReminiscencePledge where
  getAbilities (ReminiscencePledge a) =
    [ mkAbility a 1 $ forced $ oneOf [GameEnds #when, InvestigatorEliminated #when You]
    , mkAbility a 2 $ freeReaction $ EnemyWouldBeDefeated #when (EnemyAt $ here a)
    ]

-- The card lives hidden in its holder's hand, so @inThreatAreaOf@ is Nothing and
-- scoping off it alone would leave the ability matching every location.
here :: TreacheryAttrs -> LocationMatcher
here a = case a.placement of
  HiddenInHand iid -> locationWithInvestigator iid
  InThreatArea iid -> locationWithInvestigator iid
  _ -> Anywhere

instance RunMessage ReminiscencePledge where
  runMessage msg t@(ReminiscencePledge attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      addHiddenToHand iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      addToVictory iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      -- "heal all damage from it instead"
      selectOne (EnemyAt (here attrs) <> EnemyWithDamage (atLeast 1)) >>= traverse_ \eid -> do
        damage <- field EnemyDamage eid
        healDamage eid (attrs.ability 2) damage
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> ReminiscencePledge <$> liftRunMessage msg attrs
