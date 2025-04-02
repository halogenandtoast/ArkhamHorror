module Arkham.Act.Cards.Dormancy (dormancy) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.EdgeOfTheEarth.Seal
import Arkham.Helpers.Investigator
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype Dormancy = Dormancy ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dormancy :: ActCard Dormancy
dormancy = act (1, A) Dormancy Cards.dormancy Nothing

instance HasAbilities Dormancy where
  getAbilities (Dormancy a) =
    extend
      a
      [ restricted a 1 (exists $ "The Gate of Y'quaa" <> LocationWithAnyActiveSeal)
          $ Objective
          $ forced AnyWindow
      ]

instance RunMessage Dormancy where
  runMessage msg a@(Dormancy attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      lid <- selectJust $ location_ "The Gate of Y'quaa"
      mseal <- headMay . filter (\s -> s.active) . toList <$> field LocationSeals lid
      for_ mseal \(Seal kind _ miid) -> for_ miid \iid -> case kind of
        SealA -> gainResources iid attrs 3
        SealB -> gainActions iid attrs 1
        SealC -> whenM (canHaveHorrorHealed attrs iid) $ healHorror iid attrs 1
        SealD -> whenM (canHaveDamageHealed attrs iid) $ healDamage iid attrs 1
        SealE -> drawCards iid attrs 2

      advanceActDeck attrs
      pure a
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    _ -> Dormancy <$> liftRunMessage msg attrs
