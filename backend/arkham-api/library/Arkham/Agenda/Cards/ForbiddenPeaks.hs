{-# OPTIONS_GHC -Wno-deprecations #-}

module Arkham.Agenda.Cards.ForbiddenPeaks (forbiddenPeaks) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (terror)
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.DamageEffect (nonAttack)
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Message (pushWhen)
import Arkham.Helpers.Text
import Arkham.I18n
import Arkham.Matcher hiding (AssetDefeated)
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Placement
import Arkham.Scenarios.ToTheForbiddenPeaks.Helpers
import Arkham.Trait (Trait (Expedition))

newtype ForbiddenPeaks = ForbiddenPeaks AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forbiddenPeaks :: AgendaCard ForbiddenPeaks
forbiddenPeaks = agenda (1, A) ForbiddenPeaks Cards.forbiddenPeaks (Static 6)

instance HasAbilities ForbiddenPeaks where
  getAbilities (ForbiddenPeaks a) =
    [ mkAbility a 1 $ forced $ Moves #after (You <> InvestigatorWithAnyClues) AnySource Anywhere Anywhere
    ]

instance RunMessage ForbiddenPeaks where
  runMessage msg a@(ForbiddenPeaks attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      selectForMaybeM (HighestRow $ LocationWithInvestigator Anyone) \loc -> do
        selectEach (AssetControlledBy (investigatorAt loc) <> withTrait Expedition) (`place` loc)
        terror <- createSetAsideEnemy Enemies.terrorOfTheStarsGuardianOfForbiddenPeaks loc

        partners <- getPartnersWithStatus (== Safe)

        for_ (nonEmpty partners) \ps -> do
          x <- sample ps
          let isCookie = x.cardCode == Assets.jamesCookieFredericksDubiousChoice.cardCode
          scenarioI18n
            $ scope "interlude"
            $ story
            $ withVars ["isCookie" .= String (if isCookie then "valid" else "invalid")]
            $ i18nWithTitle "tragedyStrikes"
          setPartnerStatus x Eliminated
          selectForMaybeM
            (assetIs x.cardCode)
            (push . AssetDefeated (toSource attrs))

          pushWhen isCookie $ Msg.EnemyDamage terror (nonAttack (toSource attrs) 2)

      advanceAgendaDeck attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discardAllClues (attrs.ability 1) iid
      pure a
    _ -> ForbiddenPeaks <$> liftRunMessage msg attrs
