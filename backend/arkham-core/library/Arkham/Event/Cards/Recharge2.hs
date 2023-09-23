module Arkham.Event.Cards.Recharge2 (
  recharge2,
  Recharge2 (..),
) where

import Arkham.Prelude

import Arkham.Asset.Uses
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.RequestedChaosTokenStrategy
import Arkham.Trait hiding (Cultist)
import Arkham.Window qualified as Window

newtype Meta = Meta {chosenAsset :: Maybe AssetId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Recharge2 = Recharge2 (EventAttrs `With` Meta)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

recharge2 :: EventCard Recharge2
recharge2 = event (Recharge2 . (`With` Meta Nothing)) Cards.recharge2

instance RunMessage Recharge2 where
  runMessage msg e@(Recharge2 (attrs `With` meta)) = case msg of
    InvestigatorPlayEvent iid eid _ windows' _ | eid == toId attrs -> do
      assets <-
        selectListMap AssetTarget
          $ AssetControlledBy
            (InvestigatorAt $ LocationWithInvestigator $ InvestigatorWithId iid)
          <> AssetOneOf [AssetWithTrait Spell, AssetWithTrait Relic]
      push
        $ chooseOne
          iid
          [ TargetLabel target [ResolveEvent iid eid (Just target) windows']
          | target <- assets
          ]
      pure e
    ResolveEvent iid eid (Just (AssetTarget aid)) _ | eid == toId attrs -> do
      pushAll [RequestChaosTokens (toSource attrs) (Just iid) (Reveal 1) SetAside]
      pure $ Recharge2 $ attrs `with` Meta (Just aid)
    RequestedChaosTokens source _ tokens | isSource attrs source -> do
      push $ ResetChaosTokens (toSource attrs)
      case chosenAsset meta of
        Nothing -> error "invalid use"
        Just aid -> do
          if any
            ( (`elem` [Skull, Cultist, Tablet, ElderThing, AutoFail])
                . chaosTokenFace
            )
            tokens
            then
              push
                $ If
                  ( Window.RevealChaosTokenEventEffect
                      (eventOwner attrs)
                      tokens
                      (toId attrs)
                  )
                  [Discard (toSource attrs) $ AssetTarget aid]
            else push (AddUses aid Charge 3)
          pure e
    _ -> Recharge2 . (`with` meta) <$> runMessage msg attrs
