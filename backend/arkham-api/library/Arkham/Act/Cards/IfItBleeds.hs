module Arkham.Act.Cards.IfItBleeds (ifItBleeds) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Campaigns.TheScarletKeys.Concealed.Kind
import Arkham.Campaigns.TheScarletKeys.Concealed.Matcher
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Location (withLocationOf)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Token
import Arkham.Trait (Trait (Wilderness))

newtype IfItBleeds = IfItBleeds ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ifItBleeds :: ActCard IfItBleeds
ifItBleeds = act (2, A) IfItBleeds Cards.ifItBleeds Nothing

instance HasAbilities IfItBleeds where
  getAbilities = actAbilities \a ->
    [ restricted a 1 (youExist $ at_ $ withTrait Wilderness <> not_ (LocationWithToken ScoutingReport))
        $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) YourLocation)
    , mkAbility a 2 $ Objective $ freeReaction $ RoundEnds #when
    ]

instance RunMessage IfItBleeds where
  runMessage msg a@(IfItBleeds attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid $ placeTokensOn (attrs.ability 1) ScoutingReport 1
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      lead <- getLead
      voidChimera <- fetchCard Enemies.voidChimeraTrueForm
      drawCard lead voidChimera
      doStep 1 msg
      advanceActDeck attrs
      pure a
    DoStep 1 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      chimeraMiniCards <-
        select
          $ mapOneOf
            ConcealedCardIs
            [ VoidChimeraTrueForm
            , DecoyVoidChimeraFellbeak
            , DecoyVoidChimeraFellhound
            , DecoyVoidChimeraEarsplitter
            , DecoyVoidChimeraGorefeaster
            ]
          <> ConcealedCardAt (LocationWithToken ScoutingReport)

      lead <- getLead
      withI18n $ chooseSomeM' lead "done" do
        for_ chimeraMiniCards \card ->
          withLocationOf card.id \loc -> targeting loc $ revealConcealed lead attrs card.id
      pure a
    _ -> IfItBleeds <$> liftRunMessage msg attrs
