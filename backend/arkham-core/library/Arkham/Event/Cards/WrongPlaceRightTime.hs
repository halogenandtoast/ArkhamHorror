module Arkham.Event.Cards.WrongPlaceRightTime (
  wrongPlaceRightTime,
  WrongPlaceRightTime (..),
)
where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted hiding (InvestigatorDamage)
import Arkham.Helpers.Message (handleTargetChoice)
import Arkham.Helpers.Query (getPlayer)
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (AssetDefeated)
import Arkham.Message qualified as Msg
import Arkham.Projection
import Arkham.Strategy

newtype Meta = Meta {chosenAssets :: [AssetId]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype WrongPlaceRightTime = WrongPlaceRightTime (EventAttrs `With` Meta)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wrongPlaceRightTime :: EventCard WrongPlaceRightTime
wrongPlaceRightTime =
  eventWith
    (WrongPlaceRightTime . (`with` Meta []))
    Cards.wrongPlaceRightTime
    (afterPlayL .~ RemoveThisFromGame)

instance RunMessage WrongPlaceRightTime where
  runMessage msg e@(WrongPlaceRightTime (With attrs meta)) = runQueueT $ case msg of
    PlayThisEvent _iid (is attrs -> True) -> do
      doStep 5 msg
      pure e
    DoStep 0 (PlayThisEvent _iid (is attrs -> True)) -> do
      for_ (nub $ chosenAssets meta) \asset -> do
        push $ CheckDefeated (toSource attrs) (toTarget asset)
      pure e
    DoStep n msg'@(PlayThisEvent iid (is attrs -> True)) | n > 0 -> do
      dmg <- field InvestigatorDamage iid
      hrr <- field InvestigatorHorror iid
      canDamageAssets <- select $ AssetAt (locationWithInvestigator iid) <> AssetWithAnyRemainingHealth
      canHorrorAssets <- select $ AssetAt (locationWithInvestigator iid) <> AssetWithAnyRemainingSanity

      when ((dmg > 0 && notNull canDamageAssets) || (hrr > 0 && notNull canHorrorAssets)) do
        player <- getPlayer iid
        chooseOne iid
          $ Label "Done moving damage/horror" [DoStep 0 msg']
          : [ DamageLabel
              iid
              [ Msg.chooseOne
                  player
                  [ targetLabel
                    asset
                    [ handleTargetChoice iid attrs asset
                    , MoveTokensNoDefeated (toSource attrs) (toSource iid) (toTarget asset) #damage 1
                    , DoStep (n - 1) msg'
                    ]
                  | asset <- canDamageAssets
                  ]
              ]
            | dmg > 0 && notNull canDamageAssets
            ]
            <> [ HorrorLabel
                iid
                [ Msg.chooseOne
                    player
                    [ targetLabel
                      asset
                      [ handleTargetChoice iid attrs asset
                      , MoveTokensNoDefeated (toSource attrs) (toSource iid) (toTarget asset) #horror 1
                      , DoStep (n - 1) msg'
                      ]
                    | asset <- canHorrorAssets
                    ]
                ]
               | hrr > 0 && notNull canHorrorAssets
               ]

      pure e
    HandleTargetChoice _iid (isSource attrs -> True) (AssetTarget aid) -> do
      pure . WrongPlaceRightTime $ attrs `with` Meta (aid : chosenAssets meta)
    AssetDefeated (isSource attrs -> True) _ -> do
      drawCardsIfCan attrs.controller attrs 1
      pure e
    _ -> WrongPlaceRightTime . (`with` meta) <$> liftRunMessage msg attrs
