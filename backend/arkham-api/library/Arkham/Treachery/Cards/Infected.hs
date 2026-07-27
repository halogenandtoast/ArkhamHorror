module Arkham.Treachery.Cards.Infected (infected) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Trait (Trait (Stowaway))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Infected = Infected TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

infected :: TreacheryCard Infected
infected = treachery Infected Cards.infected

instance HasAbilities Infected where
  getAbilities (Infected a) =
    [ restricted a 1 (InThreatAreaOf You)
        $ forced
        $ TurnEnds
          #after
          ( You
              <> at_ (oneOf [LocationWithInvestigator (not_ You), LocationWithEnemy (EnemyWithTrait Stowaway)])
          )
    , skillTestAbility $ restricted a 2 (InThreatAreaOf You) actionAbility
    ]

instance RunMessage Infected where
  runMessage msg t@(Infected attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      -- "Limit 1 per investigator."
      other <- selectAny $ TreacheryInThreatAreaOf (be iid) <> treacheryIs Cards.infected
      if other then toDiscard attrs attrs else placeInThreatArea attrs iid
      pure t
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      assignDamage iid (attrs.ability 2) 1
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> Infected <$> liftRunMessage msg attrs
