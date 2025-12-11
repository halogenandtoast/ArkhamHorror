module Arkham.Enemy.Cards.ProtoplasmicReassembler (protoplasmicReassembler) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers (unDeck)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Projection

newtype ProtoplasmicReassembler = ProtoplasmicReassembler EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

protoplasmicReassembler :: EnemyCard ProtoplasmicReassembler
protoplasmicReassembler =
  enemy ProtoplasmicReassembler Cards.protoplasmicReassembler (2, Static 6, 2) (1, 2)
    & setSpawnAt (FarthestLocationFromAll Anywhere)

instance HasAbilities ProtoplasmicReassembler where
  getAbilities (ProtoplasmicReassembler a) =
    extend1 a
      $ restricted a 1 (thisExists a ReadyEnemy <> exists (NearestToEnemy (be a)))
      $ forced
      $ PhaseEnds #when #enemy

instance RunMessage ProtoplasmicReassembler where
  runMessage msg e@(ProtoplasmicReassembler attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      investigators <- select $ NearestToEnemy (be attrs)
      leadChooseOneM do
        targets investigators \iid -> handleTarget iid (attrs.ability 1) iid
      pure e
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) _ -> do
      withLocationOf attrs \loc -> do
        whenMatch iid (investigator_ can.manipulate.deck) do
          deck <- fieldMap InvestigatorDeck unDeck iid
          let cards = take 1 deck
          let golems = map (\card -> PlayerCard $ card {pcCardCode = "xgolem"}) cards
          pushAll $ map (RemovePlayerCardFromGame False . PlayerCard) cards
          for_ golems (`createEnemy_` AtLocation loc)
      pure e
    _ -> ProtoplasmicReassembler <$> liftRunMessage msg attrs
