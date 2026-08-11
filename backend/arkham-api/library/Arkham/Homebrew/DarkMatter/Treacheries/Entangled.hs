module Arkham.Homebrew.DarkMatter.Treacheries.Entangled (entangled) where

import Arkham.Ability
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Projection
import Arkham.Treachery.Import.Lifted
import Arkham.Window (windowType)
import Arkham.Window qualified as Window

newtype Entangled = Entangled TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

entangled :: TreacheryCard Entangled
entangled = treachery Entangled Cards.entangled

{- | "Forced - After attached investigator or enemy takes any amount of damage
from a source other than Entangled: Each other investigator or enemy with a copy
of Entangled attached also takes that amount of damage. Then, discard Entangled."
-}
instance HasAbilities Entangled where
  getAbilities (Entangled a) =
    [ mkAbility a 1 $ forced $ case a.placement of
        InThreatArea iid -> InvestigatorTakeDamage #after (InvestigatorWithId iid) (NotSource $ SourceIs $ toSource a)
        AttachedToEnemy eid ->
          EnemyTakeDamage
            #after
            AnyDamageEffect
            (EnemyWithId eid)
            AnyValue
            (NotSource $ SourceIs $ toSource a)
        _ -> NotAnyWindow
    ]

instance RunMessage Entangled where
  runMessage msg t@(Entangled attrs) = runQueueT $ case msg of
    {- "Revelation - Attach Entangled to an investigator or enemy without a copy of
    Entangled attached." -}
    Revelation iid (isSource attrs -> True) -> do
      -- no matcher exists for "already has a copy attached", so the occupied
      -- hosts are derived from the placements of the copies already in play
      existing <- select $ treacheryIs Cards.entangled <> not_ (TreacheryWithId attrs.id)
      taken <- traverse (field TreacheryPlacement) existing
      let occupiedBy p = p `elem` taken
      investigators <- filter (not . occupiedBy . InThreatArea) <$> select UneliminatedInvestigator
      enemies <- filter (not . occupiedBy . AttachedToEnemy) <$> select AnyEnemy
      chooseOneM iid do
        targets investigators \victim -> placeTreachery attrs (InThreatArea victim)
        targets enemies \eid -> placeTreachery attrs (AttachedToEnemy eid)
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 ws _ -> do
      let dealt = sum [n | w <- ws, Window.TakeDamage _ _ _ n <- [windowType w]]
      -- every *other* copy's host suffers the same
      others <- select $ treacheryIs Cards.entangled <> not_ (TreacheryWithId attrs.id)
      for_ others \other -> do
        placement <- field TreacheryPlacement other
        case placement of
          InThreatArea victim -> assignDamage victim (attrs.ability 1) dealt
          AttachedToEnemy eid -> nonAttackEnemyDamage Nothing (attrs.ability 1) dealt eid
          _ -> pure ()
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> Entangled <$> liftRunMessage msg attrs
