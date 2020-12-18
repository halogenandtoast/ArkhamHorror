{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Investigator.Cards.WendyAdams
  ( WendyAdams(..)
  , wendyAdams
  )
where

import Arkham.Import

import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype WendyAdams = WendyAdams Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance HasModifiersFor env WendyAdams where
  getModifiersFor source target (WendyAdams attrs) =
    getModifiersFor source target attrs

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

instance HasTokenValue env WendyAdams where
  getTokenValue (WendyAdams attrs) iid ElderSign | iid == investigatorId attrs =
    pure $ TokenValue ElderSign (PositiveModifier 0)
  getTokenValue (WendyAdams attrs) iid token = getTokenValue attrs iid token

instance ActionRunner env => HasActions env WendyAdams where
  getActions iid (WhenRevealToken You token) (WendyAdams attrs@Attrs {..})
    | iid == investigatorId = do
      let
        ability = (mkAbility
                    (toSource attrs)
                    1
                    (ReactionAbility (WhenRevealToken You token)
                    $ HandDiscardCost 1 Nothing mempty
                    )
                  )
          { abilityLimit = PerTestOrAbility
          }
      usedAbilities <- map unUsedAbility <$> getList ()
      pure
        [ ActivateCardAbilityAction investigatorId ability
        | (investigatorId, ability) `notElem` usedAbilities && not
          (null $ discardableCards attrs)
        ]
  getActions i window (WendyAdams attrs) = getActions i window attrs

instance (InvestigatorRunner env) => RunMessage env WendyAdams where
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
            , DrawAnotherToken iid
            ]
        _ -> error "we expect resolve token to be on the stack"
    When (DrawToken iid token) | iid == investigatorId -> i <$ unshiftMessages
      [ FocusTokens [token]
      , CheckWindow investigatorId [WhenDrawToken You token]
      , UnfocusTokens
      ]
    ResolveToken _drawnToken ElderSign iid | iid == investigatorId -> do
      maid <- getId @(Maybe AssetId) (CardCode "01014")
      i <$ when (isJust maid) (unshiftMessage PassSkillTest)
    _ -> WendyAdams <$> runMessage msg attrs
