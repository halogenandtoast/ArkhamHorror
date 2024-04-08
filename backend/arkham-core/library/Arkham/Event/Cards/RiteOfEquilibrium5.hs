module Arkham.Event.Cards.RiteOfEquilibrium5 (riteOfEquilibrium5, RiteOfEquilibrium5 (..)) where

import Arkham.Asset.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.ChaosBag
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher

newtype RiteOfEquilibrium5 = RiteOfEquilibrium5 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riteOfEquilibrium5 :: EventCard RiteOfEquilibrium5
riteOfEquilibrium5 = event RiteOfEquilibrium5 Cards.riteOfEquilibrium5

instance RunMessage RiteOfEquilibrium5 where
  runMessage msg e@(RiteOfEquilibrium5 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      x1 <- min <$> getRemainingCurseTokens <*> getRemainingBlessTokens
      x2 <- min <$> selectCount (ChaosTokenFaceIs #curse) <*> selectCount (ChaosTokenFaceIs #bless)
      investigatorHorror <-
        selectSum InvestigatorHorror $ HealableInvestigator (toSource attrs) #horror (colocatedWith iid)
      assetHorror <-
        selectSum AssetHorror
          $ HealableAsset (toSource attrs) #horror (AssetAt $ locationWithInvestigator iid)

      let x3 = min x2 (investigatorHorror + assetHorror)

      chooseOrRunOne iid
        $ [ Label "Add X {curse} tokens to the chaos bag to add X {bless} tokens to the chaos bag." [] | x1 > 0
          ]
        <> [ Label
            "Remove X {curse} and X {bless} tokens from the chaos bag to heal X total horror from among cards at your location."
            []
           | x3 > 0
           ]

      pure e
    _ -> RiteOfEquilibrium5 <$> lift (runMessage msg attrs)
