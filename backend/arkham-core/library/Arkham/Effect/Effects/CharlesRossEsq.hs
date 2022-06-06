module Arkham.Effect.Effects.CharlesRossEsq
  ( CharlesRossEsq(..)
  , charlesRossEsq
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Game.Helpers
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Source
import Arkham.Target
import Arkham.Trait

newtype CharlesRossEsq = CharlesRossEsq EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

charlesRossEsq :: EffectArgs -> CharlesRossEsq
charlesRossEsq = CharlesRossEsq . uncurry4 (baseAttrs "03149")

instance
  ( HasId LocationId env InvestigatorId
  , HasId LocationId env AssetId
  )
  => HasModifiersFor CharlesRossEsq where
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
  => RunMessage CharlesRossEsq where
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
