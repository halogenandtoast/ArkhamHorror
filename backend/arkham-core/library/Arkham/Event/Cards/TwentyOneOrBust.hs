{-# LANGUAGE MultiWayIf #-}

module Arkham.Event.Cards.TwentyOneOrBust (
  twentyOneOrBust,
  TwentyOneOrBust (..),
)
where

import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.RequestedChaosTokenStrategy

newtype TwentyOneOrBust = TwentyOneOrBust EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twentyOneOrBust :: EventCard TwentyOneOrBust
twentyOneOrBust = event TwentyOneOrBust Cards.twentyOneOrBust

currentTotal :: [ChaosTokenFace] -> (Int, Int)
currentTotal faces =
  let toValue = \case
        Skull -> (Sum 5, Sum 5)
        Cultist -> (Sum 5, Sum 5)
        Tablet -> (Sum 5, Sum 5)
        ElderThing -> (Sum 5, Sum 5)
        AutoFail -> (Sum 10, Sum 10)
        ElderSign -> (Sum 1, Sum 11)
        other -> let x = abs (chaosTokenToFaceValue other) in (Sum x, Sum x)
      total = foldMap toValue faces
   in bimap getSum getSum total

instance RunMessage TwentyOneOrBust where
  runMessage msg e@(TwentyOneOrBust attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      push $ RequestChaosTokens (toSource attrs) (Just iid) (Reveal 1) SetAside
      pure . TwentyOneOrBust $ attrs & setMeta @[ChaosTokenFace] []
    RequestedChaosTokens (isSource attrs -> True) (Just iid) tokens -> do
      let hand = toResult @[ChaosTokenFace] attrs.meta
          tokenFaces = map (.face) tokens
          (totalA, totalB) = currentTotal (tokenFaces <> hand)
          totalLabel = if totalA == totalB then tshow totalA else tshow totalA <> " or " <> tshow totalB
      chooseOne
        iid
        [ Label ("Stop with " <> totalLabel) [DoStep 0 msg]
        , Label "Continue" [RequestChaosTokens (toSource attrs) (Just iid) (Reveal 1) SetAside]
        ]
      pure . TwentyOneOrBust $ attrs & setMeta @[ChaosTokenFace] (tokenFaces <> hand)
    DoStep 0 msg'@(RequestedChaosTokens (isSource attrs -> True) (Just iid) _) -> do
      let hand = toResult @[ChaosTokenFace] attrs.meta
      let (totalA, totalB) = currentTotal hand
      let hasElderSign = ElderSign `elem` (toResult @[ChaosTokenFace] attrs.meta)
      chooseOrRunOne iid
        $ Label (tshow totalA) [DoStep 1 msg']
        : [Label (tshow totalB) [DoStep 11 msg'] | hasElderSign]
      pure e
    DoStep n (RequestedChaosTokens (isSource attrs -> True) (Just iid) _) -> do
      let hand = toResult @[ChaosTokenFace] attrs.meta
      let (totalA, totalB) = currentTotal hand
      let total = if n == 1 then totalA else totalB
      if
        | total <= 18 -> gainResourcesIfCan iid attrs 4
        | total == 19 -> gainResourcesIfCan iid attrs 5
        | total == 20 -> gainResourcesIfCan iid attrs 6
        | total == 21 -> gainResourcesIfCan iid attrs 9
        | otherwise -> pure ()
      push $ ResetChaosTokens (toSource attrs)
      pure e
    _ -> TwentyOneOrBust <$> lift (runMessage msg attrs)
