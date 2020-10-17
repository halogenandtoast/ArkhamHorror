{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.WendyAdams where

import Arkham.Types.Ability
import Arkham.Types.AssetId
import Arkham.Types.Card.CardCode
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import Arkham.Types.Window
import ClassyPrelude
import Data.Aeson

newtype WendyAdams = WendyAdams Attrs
  deriving newtype (Show, ToJSON, FromJSON)

wendyAdams :: WendyAdams
wendyAdams = WendyAdams $ baseAttrs
  "01005"
  "Wendy Adams"
  Survivor
  Stats
    { health = 7
    , sanity = 7
    , willpower = 4
    , intellect = 3
    , combat = 1
    , agility = 4
    }
  [Drifter]

instance ActionRunner env => HasActions env WendyAdams where
  getActions iid (WhenRevealToken You token) (WendyAdams attrs@Attrs {..})
    | iid == investigatorId = do
      let
        ability = (mkAbility
                    (InvestigatorSource investigatorId)
                    1
                    (ReactionAbility (WhenRevealToken You token))
                  )
          { abilityLimit = PerTestOrAbility
          }
      usedAbilities <- map unUsedAbility <$> asks (getList ())
      pure
        [ ActivateCardAbilityAction investigatorId ability
        | (investigatorId, ability) `notElem` usedAbilities && not
          (null $ discardableCards attrs)
        ]
  getActions i window (WendyAdams attrs) = getActions i window attrs

instance (InvestigatorRunner Attrs env) => RunMessage env WendyAdams where
  runMessage msg i@(WendyAdams attrs@Attrs {..}) = case msg of
    UseCardAbility _ (InvestigatorSource iid) _ 1 | iid == investigatorId -> do
      mResolveToken <- withQueue $ \queue ->
        (queue, find ((== Just RevealTokenMessage) . messageType) queue)
      case mResolveToken of
        Just (RevealToken _ _ token) -> do
          void
            $ withQueue
            $ \queue ->
                ( filter (/= CheckWindow iid [WhenRevealToken You token]) queue
                , ()
                )
          i <$ unshiftMessages
            [ ChooseAndDiscardCard iid
            , CancelNext DrawTokenMessage
            , CancelNext RevealTokenMessage
            , ReturnTokens [token]
            , UnfocusTokens
            , DrawAnotherToken iid 0
            ]
        _ -> error "we expect resolve token to be on the stack"
    When (DrawToken iid token) | iid == investigatorId -> i <$ unshiftMessages
      [ FocusTokens [token]
      , CheckWindow investigatorId [WhenDrawToken You token]
      , UnfocusTokens
      ]
    ResolveToken ElderSign iid | iid == investigatorId -> do
      maid <- asks (getId @(Maybe AssetId) (CardCode "01014"))
      case maid of
        Nothing -> i <$ runTest investigatorId (TokenValue ElderSign 0)
        Just _ -> i <$ unshiftMessage PassSkillTest
    _ -> WendyAdams <$> runMessage msg attrs
