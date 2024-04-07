module Arkham.Asset.Cards.Geas2 (geas2, Geas2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Matcher hiding (PlayCard)

newtype Geas2 = Geas2 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

geas2 :: AssetCard Geas2
geas2 = assetWith Geas2 Cards.geas2 $ setMeta @Promise 0

type Promise = Int

pattern DrawPromise :: Promise
pattern DrawPromise <- 1 where DrawPromise = 1

pattern PlayPromise :: Promise
pattern PlayPromise <- 2 where PlayPromise = 2

pattern CommitPromise :: Promise
pattern CommitPromise <- 3 where CommitPromise = 3

instance HasModifiersFor Geas2 where
  getModifiersFor (InvestigatorTarget iid) (Geas2 a) | a `controlledBy` iid = do
    pure $ toModifiers a [SkillModifier sType 1 | sType <- [#willpower, #intellect, #combat, #agility]]
  getModifiersFor _ _ = pure []

instance HasAbilities Geas2 where
  getAbilities (Geas2 a) =
    [restrictedAbility a 1 ControlsThis $ forced $ AssetEntersPlay #when (be a)]

checkPromise
  :: (ReverseQueue m, HasGameLogger m) => AssetAttrs -> InvestigatorId -> Promise -> m AssetAttrs
checkPromise attrs iid promise = do
  isTurn <- iid <=~> TurnInvestigator
  if isTurn && toResult @Promise attrs.meta == promise
    then do
      n <- min 10 <$> getRemainingCurseTokens
      send "You have broken the geas!"
      toDiscardBy iid attrs attrs
      replicateM_ n $ addChaosToken #curse
      pure $ setMeta @Promise 0 attrs
    else pure attrs

instance RunMessage Geas2 where
  runMessage msg a@(Geas2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOne
        iid
        [ Label "I shall not DRAW any cards during each of my turns" [DoStep DrawPromise msg]
        , Label "I shall not PLAY any cards during each of my turns" [DoStep PlayPromise msg]
        , Label "I shall not COMMIT any cards during each of my turns" [DoStep CommitPromise msg]
        ]
      pure a
    DoStep n (UseThisAbility _iid (isSource attrs -> True) 1) -> do
      pure $ Geas2 $ setMeta n attrs
    CommitCard iid _ | Just iid == attrs.controller -> do
      Geas2 <$> checkPromise attrs iid CommitPromise
    DrawCards cardDraw | Just cardDraw.investigator == attrs.controller -> do
      Geas2 <$> checkPromise attrs cardDraw.investigator DrawPromise
    PlayCard iid _ _ _ _ True | Just iid == attrs.controller -> do
      Geas2 <$> checkPromise attrs iid PlayPromise
    _ -> Geas2 <$> lift (runMessage msg attrs)
