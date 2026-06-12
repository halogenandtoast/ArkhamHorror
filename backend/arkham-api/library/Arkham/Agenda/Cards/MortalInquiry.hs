module Arkham.Agenda.Cards.MortalInquiry (mortalInquiry) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Scenarios.ReadOrDie.Helpers
import Arkham.Slot
import Arkham.Trait (Trait (Miskatonic, Tome))

newtype MortalInquiry = MortalInquiry AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mortalInquiry :: AgendaCard MortalInquiry
mortalInquiry = agenda (1, A) MortalInquiry Cards.mortalInquiry (Static 12)

instance HasModifiersFor MortalInquiry where
  getModifiersFor (MortalInquiry a) = when (onSide A a) do
    modifySelect a (assetIs Assets.drHenryArmitage) [DoNotTakeUpSlot #ally]

instance HasAbilities MortalInquiry where
  getAbilities (MortalInquiry a) =
    guard (onSide A a)
      *> [ scenarioI18n
             $ withI18nTooltip "mortalInquiry.moveJazz"
             $ restricted (proxied (assetIs Assets.jazzMulligan) a) 1 (Uncontrolled <> OnSameLocation) actionAbility
         ]

instance RunMessage MortalInquiry where
  runMessage msg a@(MortalInquiry attrs) = runQueueT $ case msg of
    TakeControlOfAsset iid aid -> do
      whenM (aid <=~> assetIs Assets.drHenryArmitage) do
        pushAll $ replicate 2 $ AddSlot iid #hand (TraitRestrictedSlot (AssetSource aid) Tome [])
      MortalInquiry <$> liftRunMessage msg attrs
    UseThisAbility iid (ProxySource _ (isSource attrs -> True)) 1 -> do
      jazz <- selectJust $ assetIs Assets.jazzMulligan
      locations <-
        select
          $ LocationWithTrait Miskatonic
          <> not_ (locationWithAsset jazz)
          <> LocationWithDistanceFromAtMost 3 (locationWithAsset jazz) (LocationWithTrait Miskatonic)
      chooseTargetM iid locations \lid -> push $ PlaceAsset jazz (AtLocation lid)
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R2
      pure a
    _ -> MortalInquiry <$> liftRunMessage msg attrs
