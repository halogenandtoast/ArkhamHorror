module Arkham.Agenda.Cards.ForbiddenPeaks (forbiddenPeaks) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (terror)
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Text
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Scenarios.ToTheForbiddenPeaks.Helpers

newtype ForbiddenPeaks = ForbiddenPeaks AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forbiddenPeaks :: AgendaCard ForbiddenPeaks
forbiddenPeaks = agenda (1, A) ForbiddenPeaks Cards.forbiddenPeaks (Static 6)

instance HasAbilities ForbiddenPeaks where
  getAbilities (ForbiddenPeaks a) =
    [mkAbility a 1 $ forced $ Moves #after You AnySource Anywhere Anywhere]

instance RunMessage ForbiddenPeaks where
  runMessage msg a@(ForbiddenPeaks attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      -- Put the set-aside Terror of the Stars enemy into play at the highest level location with one or more investigators. Each investigator at that location loses control of all Expedition assets and places them at their location.

      -- Read Scenario Interlude: Tragedy Strikes in the Campaign Guide.
      selectForMaybeM (HighestRow $ LocationWithInvestigator Anyone) \loc -> do
        selectEach (AssetControlledBy (investigatorAt loc)) (`place` loc)
        terror <- createSetAsideEnemy Enemies.terrorOfTheStarsGuardianOfForbiddenPeaks loc

        partners <- getPartnersWithStatus (== Safe)

        for_ (nonEmpty partners) \ps -> do
          x <- sample ps
          let isCookie = x.cardCode == Assets.jamesCookieFredericksDubiousChoice.cardCode
          scenarioI18n
            $ scope "interlude"
            $ story
            $ FlavorText
              Nothing
              [ i18nEntry "tragedyStrikes.part1"
                  <> validateEntry isCookie "tragedyStrikes.cookie"
                  <> i18nEntry "tragedyStrikes.part2"
              ]
          setPartnerStatus x Eliminated
          when isCookie $ nonAttackEnemyDamage attrs 2 terror

      advanceAgendaDeck attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discardAllClues (attrs.ability 1) iid
      pure a
    _ -> ForbiddenPeaks <$> liftRunMessage msg attrs
