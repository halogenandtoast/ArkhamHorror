module Arkham.Act.Cards.HotOnYourTail (hotOnYourTail) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted hiding (InvestigatorResigned)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Investigator.Types (Field (InvestigatorResources))
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait (Trait (Criminal))
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype HotOnYourTail = HotOnYourTail ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hotOnYourTail :: ActCard HotOnYourTail
hotOnYourTail = act (2, A) HotOnYourTail Cards.hotOnYourTail Nothing

instance HasModifiersFor HotOnYourTail where
  getModifiersFor (HotOnYourTail a) = do
    modifySelect a (EnemyWithTrait Criminal) [AddKeyword Keyword.Hunter, HealthModifier 1]

instance HasAbilities HotOnYourTail where
  getAbilities = actAbilities \a ->
    [ mkAbility a 1 $ forced $ InvestigatorResigned #when Anyone
    , restricted a 2 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow
    ]

instance RunMessage HotOnYourTail where
  runMessage msg a@(HotOnYourTail attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws _ -> do
      for_ [iid | (windowType -> Window.InvestigatorResigned iid) <- ws] \iid -> do
        n <- field InvestigatorResources iid
        when (n > 0) do
          loseResources iid (attrs.ability 1) n
          scenarioSpecific "placeResourcesOnAct" n
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> HotOnYourTail <$> liftRunMessage msg attrs
