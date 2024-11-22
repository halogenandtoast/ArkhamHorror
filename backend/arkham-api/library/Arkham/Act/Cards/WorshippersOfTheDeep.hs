module Arkham.Act.Cards.WorshippersOfTheDeep (WorshippersOfTheDeep (..), worshippersOfTheDeep) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Investigator (getMaybeLocation, withLocationOf)
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Projection

newtype WorshippersOfTheDeep = WorshippersOfTheDeep ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

worshippersOfTheDeep :: ActCard WorshippersOfTheDeep
worshippersOfTheDeep = act (3, A) WorshippersOfTheDeep Cards.worshippersOfTheDeep Nothing

instance HasAbilities WorshippersOfTheDeep where
  getAbilities (WorshippersOfTheDeep a) =
    [ restrictedAbility a 1 (exists $ InvestigatorAt FullyFloodedLocation)
        $ forced
        $ PhaseEnds #when #investigation
    , restrictedAbility a 2 AllUndefeatedInvestigatorsResigned
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage WorshippersOfTheDeep where
  runMessage msg a@(WorshippersOfTheDeep attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      selectEach (InvestigatorAt FullyFloodedLocation) \iid -> do
        chooseOrRunOneM iid do
          labeled "Take 3 damage" $ assignDamage iid (attrs.ability 1) 3
          withLocationOf iid \lid -> void $ runMaybeT do
            pos <- MaybeT $ field LocationPosition lid
            below <- MaybeT $ selectOne $ LocationInPosition (Pos pos.column (pos.row - 1))
            lift
              $ labeled "Move to the location directly below their location"
              $ moveTo (attrs.ability 1) iid below
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push $ if toResultDefault False attrs.meta then R1 else R2
      pure a
    Resign iid -> do
      getMaybeLocation iid >>= \case
        Nothing -> pure a
        Just lid -> do
          isMoonRoom <- lid <=~> locationIs Locations.theMoonRoom
          pure $ WorshippersOfTheDeep $ attrs & metaL .~ toJSON isMoonRoom
    _ -> WorshippersOfTheDeep <$> liftRunMessage msg attrs
