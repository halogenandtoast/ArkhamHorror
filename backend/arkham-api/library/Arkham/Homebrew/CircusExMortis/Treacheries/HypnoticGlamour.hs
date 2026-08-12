module Arkham.Homebrew.CircusExMortis.Treacheries.HypnoticGlamour (hypnoticGlamour) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (campaignI18n, moonToken, sealMoonTokenOn)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype HypnoticGlamour = HypnoticGlamour TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hypnoticGlamour :: TreacheryCard HypnoticGlamour
hypnoticGlamour = treachery HypnoticGlamour Cards.hypnoticGlamour

instance HasAbilities HypnoticGlamour where
  getAbilities (HypnoticGlamour a) =
    [ restricted a 1 OnSameLocation $ forced $ TurnEnds #after You
    , skillTestAbility $ restricted a 2 OnSameLocation actionAbility
    ]

instance RunMessage HypnoticGlamour where
  runMessage msg t@(HypnoticGlamour attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      selectWhenNotNull
        ( LocationWithMostClues
            $ RevealedLocation
            <> LocationWithoutTreachery (treacheryIs Cards.hypnoticGlamour)
        )
        \locations -> chooseOrRunOneM iid $ targets locations $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      moonInBag <- selectAny moonToken
      chooseOneM iid $ withI18n do
        chooseTakeHorror iid attrs 1
        when moonInBag
          $ campaignI18n
          $ labeled' "sealMoonToken"
          $ sealMoonTokenOn iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) attrs #willpower (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> HypnoticGlamour <$> liftRunMessage msg attrs
