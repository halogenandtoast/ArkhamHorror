module Arkham.Treachery.Cards.LostInTheWilds (lostInTheWilds, LostInTheWilds (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LostInTheWilds = LostInTheWilds TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostInTheWilds :: TreacheryCard LostInTheWilds
lostInTheWilds = treachery LostInTheWilds Cards.lostInTheWilds

instance HasModifiersFor LostInTheWilds where
  getModifiersFor (InvestigatorTarget iid) (LostInTheWilds attrs) = do
    modified attrs
      $ guard (treacheryInThreatArea iid attrs)
      *> [CannotMove, CannotExplore]
  getModifiersFor _ _ = pure []

instance HasAbilities LostInTheWilds where
  getAbilities (LostInTheWilds a) =
    [ restrictedAbility a 1 (InThreatAreaOf You)
        $ forced
        $ TurnEnds #when You
    ]

instance RunMessage LostInTheWilds where
  runMessage msg t@(LostInTheWilds attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      assignHorror iid attrs n
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> LostInTheWilds <$> liftRunMessage msg attrs
