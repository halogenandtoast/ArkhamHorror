module Arkham.Event.Cards.RiteOfEquilibrium5 (riteOfEquilibrium5, RiteOfEquilibrium5 (..)) where

import Arkham.Asset.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Query (getPlayer)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message (questionLabel)

newtype RiteOfEquilibrium5 = RiteOfEquilibrium5 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riteOfEquilibrium5 :: EventCard RiteOfEquilibrium5
riteOfEquilibrium5 = event RiteOfEquilibrium5 Cards.riteOfEquilibrium5

instance RunMessage RiteOfEquilibrium5 where
  runMessage msg e@(RiteOfEquilibrium5 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      x1 <- min <$> getRemainingCurseTokens <*> getRemainingBlessTokens
      blessTokens <- select $ ChaosTokenFaceIs #bless
      curseTokens <- select $ ChaosTokenFaceIs #curse

      investigatorHorror <-
        selectSum InvestigatorHorror $ HealableInvestigator (toSource attrs) #horror (colocatedWith iid)
      assetHorror <-
        selectSum AssetHorror
          $ HealableAsset (toSource attrs) #horror (AssetAt $ locationWithInvestigator iid)

      let x2 = min (min (length blessTokens) (length curseTokens)) (investigatorHorror + assetHorror)

      player <- getPlayer iid

      chooseOrRunOne iid
        $ [ Label
            "Add X {curse} tokens to the chaos bag to add X {bless} tokens to the chaos bag."
            [ questionLabel "Choose X" player
                $ DropDown
                  [ (tshow x, Run (replicate x (AddChaosToken #curse) <> replicate x (AddChaosToken #bless)))
                  | x <- [1 .. x1]
                  ]
            ]
          | x1 > 0
          ]
        <> [ Label
            "Remove X {curse} and X {bless} tokens from the chaos bag to heal X total horror from among cards at your location."
            [ questionLabel "Choose X" player
                $ DropDown
                  [ ( tshow x
                    , Run
                        [ReturnChaosTokensToPool (take x curseTokens <> take x blessTokens), DoStep x msg]
                    )
                  | x <- [1 .. x2]
                  ]
            ]
           | x2 > 0
           ]

      pure e
    DoStep 0 (PlayThisEvent _iid eid) | eid == toId attrs -> do
      push $ ApplyHealing (toSource attrs)
      pure e
    DoStep n msg'@(PlayThisEvent iid eid) | eid == toId attrs -> do
      healableInvestigators <-
        selectWithField InvestigatorHorror
          $ HealableInvestigator (toSource attrs) #horror (colocatedWith iid)
      healableAssets <-
        selectWithField AssetHorror
          $ HealableAsset (toSource attrs) #horror (AssetAt $ locationWithInvestigator iid)

      if sum (map snd healableInvestigators) + sum (map snd healableAssets) == n
        then
          pushAll
            $ [ HealHorrorDelayed (toTarget investigator) (toSource attrs) x
              | (investigator, x) <- healableInvestigators
              ]
            <> [ HealHorrorDelayed (toTarget asset) (toSource attrs) x
               | (asset, x) <- healableAssets
               ]
            <> [ApplyHealing (toSource attrs)]
        else do
          let nextStep = DoStep (n - 1) msg'

          chooseOrRunOne iid
            $ [ targetLabel investigator [HealHorrorDelayed (toTarget investigator) (toSource attrs) 1, nextStep]
              | (investigator, _) <- healableInvestigators
              ]
            <> [ targetLabel asset [HealHorrorDelayed (toTarget asset) (toSource attrs) 1, nextStep]
               | (asset, _) <- healableAssets
               ]

      pure e
    _ -> RiteOfEquilibrium5 <$> lift (runMessage msg attrs)
