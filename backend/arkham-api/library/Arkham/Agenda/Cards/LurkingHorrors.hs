module Arkham.Agenda.Cards.LurkingHorrors (lurkingHorrors) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Card.CardDef
import Arkham.Helpers.Text
import Arkham.I18n
import Arkham.Matcher hiding (AssetDefeated, DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Scenarios.CityOfTheElderThings.Helpers

newtype LurkingHorrors = LurkingHorrors AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lurkingHorrors :: AgendaCard LurkingHorrors
lurkingHorrors = agenda (1, A) LurkingHorrors Cards.lurkingHorrors (Static 6)

instance HasAbilities LurkingHorrors where
  getAbilities (LurkingHorrors attrs) =
    [ withTooltip
        "During your turn, spend 1 clue: Look at the revealed side of a City Landscape in your column or row. (Limit once per round.)"
        $ playerLimit PerRound
        $ restricted
          attrs
          1
          ( DuringTurn You
              <> exists
                ( UnrevealedLocation
                    <> oneOf
                      [ LocationInRowOf (LocationWithInvestigator You)
                      , LocationInColumnOf (LocationWithInvestigator You)
                      ]
                )
          )
        $ FastAbility (ClueCost $ Static 1)
    , withTooltip
        "During your turn, spend 3 clues: Move to any location in your column or row. (Limit once per round.)"
        $ playerLimit PerRound
        $ restricted
          attrs
          2
          ( DuringTurn You
              <> exists
                ( CanEnterLocation You
                    <> oneOf
                      [LocationInRowOf (LocationWithInvestigator You), LocationInColumnOf (LocationWithInvestigator You)]
                )
          )
        $ FastAbility (ClueCost $ Static 3)
    ]

instance RunMessage LurkingHorrors where
  runMessage msg a@(LurkingHorrors attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      ls <-
        select
          $ UnrevealedLocation
          <> oneOf
            [ LocationInRowOf (locationWithInvestigator iid)
            , LocationInColumnOf (locationWithInvestigator iid)
            ]
      chooseTargetM iid ls $ lookAtRevealed iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      ls <-
        select
          $ CanMoveToLocation (InvestigatorWithId iid) (attrs.ability 2)
          $ oneOf
            [ LocationInRowOf (locationWithInvestigator iid)
            , LocationInColumnOf (locationWithInvestigator iid)
            ]
      chooseTargetM iid ls $ moveTo (attrs.ability 1) iid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> scenarioI18n $ scope "interlude" do
      partners <- getPartnersWithStatus (== Safe)
      case nonEmpty partners of
        Nothing -> story $ i18n "instructions"
        Just ps -> do
          x <- sample ps
          story $ i18n "instructions" <> i18n "part1"
          storyWithCard (toCardDef x)
            $ blueFlavor (validateEntry (x.cardCode == Assets.danforthBrilliantStudent.cardCode) "danforth")
            <> i18n "part2"
          setPartnerStatus x Eliminated
          selectForMaybeM
            (assetIs x.cardCode)
            (push . AssetDefeated (toSource attrs))

      advanceAgendaDeck attrs
      pure a
    _ -> LurkingHorrors <$> liftRunMessage msg attrs
