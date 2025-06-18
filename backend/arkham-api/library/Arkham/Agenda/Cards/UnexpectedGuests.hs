module Arkham.Agenda.Cards.UnexpectedGuests (unexpectedGuests) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Window (assetLeavingPlay)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheMidwinterGala.Helpers (becomeSpellbound)
import Arkham.Trait

newtype UnexpectedGuests = UnexpectedGuests AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unexpectedGuests :: AgendaCard UnexpectedGuests
unexpectedGuests = agenda (2, A) UnexpectedGuests Cards.unexpectedGuests (Static 6)

instance HasModifiersFor UnexpectedGuests where
  getModifiersFor (UnexpectedGuests a) = do
    when (onSide A a) $ modifySelect a Anyone [CannotParleyWith $ EnemyWithTrait LanternClub]
    modifySelf a [CannotRemoveDoomOnThis]

instance HasAbilities UnexpectedGuests where
  getAbilities (UnexpectedGuests a) =
    [ mkAbility a 1
        $ forced
        $ AssetWouldLeavePlay #when (AssetWithTrait Guest <> SingleSidedAsset)
    ]

instance RunMessage UnexpectedGuests where
  runMessage msg a@(UnexpectedGuests attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (assetLeavingPlay -> aid) _ -> do
      becomeSpellbound aid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      lead <- getLead
      selectOne (enemyIs Enemies.theBloodlessMan) >>= \case
        Just theBloodlessMan -> flipOver lead theBloodlessMan
        Nothing -> eachInvestigator (`forInvestigator` msg)
      advanceAgendaDeck attrs
      pure a
    ForInvestigator iid (AdvanceAgenda (isSide B attrs -> True)) -> do
      healableInvestigators <-
        select $ HealableInvestigator (toSource attrs) #horror (affectsOthers $ colocatedWith iid)
      healableAssets <-
        select $ HealableAsset (toSource attrs) #horror (AssetAt $ locationWithInvestigator iid)

      chooseOrRunOneM iid do
        withI18n $ labeled' "skip" nothing
        targets healableInvestigators $ healHorrorOn attrs 1
        targets healableAssets $ healHorrorOn attrs 1
      pure a
    _ -> UnexpectedGuests <$> liftRunMessage msg attrs
