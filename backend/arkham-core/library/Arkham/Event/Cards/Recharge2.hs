module Arkham.Event.Cards.Recharge2
  ( recharge2
  , Recharge2(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Asset.Uses
import Arkham.Classes
import Arkham.Event.Attrs
import Arkham.Event.Runner
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.RequestedTokenStrategy
import Arkham.Target
import Arkham.Token
import Arkham.Trait hiding (Cultist)

newtype Meta = Meta { chosenAsset :: Maybe AssetId }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Recharge2 = Recharge2 (EventAttrs `With` Meta)
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

recharge2 :: EventCard Recharge2
recharge2 = event (Recharge2 . (`With` Meta Nothing)) Cards.recharge2

instance EventRunner env => RunMessage env Recharge2 where
  runMessage msg e@(Recharge2 (attrs `With` meta)) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      lid <- getId @LocationId iid
      assets <-
        selectListMap AssetTarget
        $ AssetOwnedBy (InvestigatorAt $ LocationWithId lid)
        <> AssetOneOf [AssetWithTrait Spell, AssetWithTrait Relic]
      e <$ push
        (chooseOne
          iid
          [ TargetLabel target [ResolveEvent iid eid (Just target)]
          | target <- assets
          ]
        )
    ResolveEvent iid eid (Just (AssetTarget aid)) | eid == toId attrs -> do
      pushAll
        [ RequestTokens (toSource attrs) (Just iid) 1 SetAside
        , Discard (toTarget attrs)
        ]
      pure $ Recharge2 $ attrs `with` Meta (Just aid)
    RequestedTokens source _ tokens | isSource attrs source ->
      case chosenAsset meta of
        Nothing -> error "invalid use"
        Just aid -> do
          if any
              ((`elem` [Skull, Cultist, Tablet, ElderThing, AutoFail])
              . tokenFace
              )
              tokens
            then push (Discard $ AssetTarget aid)
            else push (AddUses (AssetTarget aid) Charge 3)
          pure e
    _ -> Recharge2 . (`with` meta) <$> runMessage msg attrs
