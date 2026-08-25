module Arkham.Homebrew.DarkMatter.Treacheries.SolarEclipse (solarEclipse) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (campaignI18n)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype SolarEclipse = SolarEclipse TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

solarEclipse :: TreacheryCard SolarEclipse
solarEclipse = treachery SolarEclipse Cards.solarEclipse

{- | "Revelation - Put Solar Eclipse into play next to the agenda deck. /
Forced - When you initiate an investigation: You must either take 1 horror, or
your location gets +2 shroud for this investigation. / Forced - At the end of the
round: Discard Solar Eclipse."
-}
instance HasAbilities SolarEclipse where
  getAbilities (SolarEclipse a) =
    [ mkAbility a 1 $ forced $ InitiatedSkillTest #when You #any #any (WhileInvestigating Anywhere)
    , mkAbility a 2 $ forced $ RoundEnds #when
    ]

instance RunMessage SolarEclipse where
  runMessage msg t@(SolarEclipse attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      placeTreachery attrs NextToAgenda
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid $ campaignI18n do
        labeled' "solarEclipse.take1Horror" $ assignHorror iid (attrs.ability 1) 1
        labeled' "solarEclipse.locationGets2Shroud" do
          here <- select $ locationWithInvestigator iid
          withSkillTest \sid ->
            for_ here \lid -> skillTestModifier sid (attrs.ability 1) lid (ShroudModifier 2)
      pure t
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      toDiscard (attrs.ability 2) attrs
      pure t
    _ -> SolarEclipse <$> liftRunMessage msg attrs
