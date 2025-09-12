module Arkham.Act.Cards.EscapeTheCage (escapeTheCage) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Card
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Trait (Trait (SilverTwilight))

newtype EscapeTheCage = EscapeTheCage ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

escapeTheCage :: ActCard EscapeTheCage
escapeTheCage = act (3, A) EscapeTheCage Cards.escapeTheCage Nothing

instance HasAbilities EscapeTheCage where
  getAbilities = actAbilities \x ->
    [ mkAbility x 1 $ forced $ RoundEnds #when
    , restricted x 2 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow
    ]

instance RunMessage EscapeTheCage where
  runMessage msg a@(EscapeTheCage attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      entryHall <- selectJust $ LocationWithTitle "Entry Hall"
      enemiesToMove <-
        select $ InPlayEnemy $ #ready <> withTrait SilverTwilight <> not_ (enemyAt entryHall) <> AloofEnemy
      select (enemyAt entryHall <> EnemyWithTrait SilverTwilight)
        >>= traverse convertToCard
        >>= placeUnderneath entryHall
      leadChooseOneAtATimeM $ targets enemiesToMove \enemy -> moveTowards attrs enemy entryHall
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    _ -> EscapeTheCage <$> liftRunMessage msg attrs
