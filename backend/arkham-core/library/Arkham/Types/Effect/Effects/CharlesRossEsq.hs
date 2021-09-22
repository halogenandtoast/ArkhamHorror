module Arkham.Types.Effect.Effects.CharlesRossEsq
  ( CharlesRossEsq(..)
  , charlesRossEsq
  ) where

import Arkham.Prelude

import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.Game.Helpers
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait

newtype CharlesRossEsq = CharlesRossEsq EffectAttrs
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

charlesRossEsq :: EffectArgs -> CharlesRossEsq
charlesRossEsq = CharlesRossEsq . uncurry4 (baseAttrs "03149")

instance
  ( HasId LocationId env InvestigatorId
  , HasId LocationId env AssetId
  )
  => HasModifiersFor env CharlesRossEsq where
  getModifiersFor (InvestigatorSource iid) (CardIdTarget _) (CharlesRossEsq attrs)
    = do
      case effectSource attrs of
        AssetSource aid -> do
          sameLocation <- liftA2
            (==)
            (getId @LocationId aid)
            (getId @LocationId iid)
          pure $ toModifiers
            attrs
            [ ReduceCostOf (CardWithType AssetType <> CardWithTrait Item) 1
            | sameLocation
            ]
        _ -> error "invalid source"
  getModifiersFor _ _ _ = pure []

instance
  ( HasId LocationId env InvestigatorId
  , HasId LocationId env AssetId
  , HasQueue env
  )
  => RunMessage env CharlesRossEsq where
  runMessage msg e@(CharlesRossEsq attrs) = case msg of
    PlayedCard iid card -> case effectSource attrs of
      AssetSource aid -> do
        sameLocation <- liftA2
          (==)
          (getId @LocationId aid)
          (getId @LocationId iid)
        if sameLocation
            && cardMatch card (CardWithType AssetType <> CardWithTrait Item)
          then e <$ push (DisableEffect $ toId attrs)
          else pure e
      _ -> error "Invalid source"
    _ -> CharlesRossEsq <$> runMessage msg attrs
