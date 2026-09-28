module Arkham.Homebrew.CircusExMortis.Acts.FashionablyEarly (fashionablyEarly) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Cards
import Arkham.Matcher

newtype FashionablyEarly = FashionablyEarly ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fashionablyEarly :: ActCard FashionablyEarly
fashionablyEarly = act (3, A) FashionablyEarly Cards.fashionablyEarly Nothing

instance HasAbilities FashionablyEarly where
  getAbilities = actAbilities1 \a ->
    onlyOnce $ restricted a 1 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow

instance RunMessage FashionablyEarly where
  runMessage msg a@(FashionablyEarly attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R2
      pure a
    _ -> FashionablyEarly <$> liftRunMessage msg attrs
