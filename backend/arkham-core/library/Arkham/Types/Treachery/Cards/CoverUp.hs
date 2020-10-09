{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.CoverUp where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId
import Arkham.Types.Window
import ClassyPrelude
import Lens.Micro
import Safe (fromJustNote)

newtype CoverUp = CoverUp Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

coverUp :: TreacheryId -> Maybe InvestigatorId -> CoverUp
coverUp uuid iid =
  CoverUp $ (weaknessAttrs uuid iid "01007") { treacheryClues = Just 3 }

instance (ActionRunner env investigator) => HasActions env investigator CoverUp where
  getActions i window@(WhenDiscoverClues You YourLocation) (CoverUp Attrs {..})
    | Just (getId () i) == treacheryAttachedInvestigator = do
      cluesToDiscover <- withQueue $ \queue -> do
        let
          mDiscoverClues = flip find queue $ \case
            DiscoverClues{} -> True
            _ -> False
          clues' = case mDiscoverClues of
            Just (DiscoverClues _ _ m) -> m
            _ -> 0
        (queue, clues')
      pure
        [ ActivateCardAbilityAction
            (getId () i)
            (mkAbility (TreacherySource treacheryId) 1 (ReactionAbility window))
        | fromJustNote "Must be set" treacheryClues > 0 && cluesToDiscover > 0
        ]
  getActions _ _ _ = pure []

instance (TreacheryRunner env) => RunMessage env CoverUp where
  runMessage msg t@(CoverUp attrs@Attrs {..}) = case msg of
    Revelation iid tid | tid == treacheryId -> do
      unshiftMessages
        [ RemoveCardFromHand iid "01007"
        , AttachTreachery tid (InvestigatorTarget iid)
        ]
      CoverUp <$> runMessage msg (attrs & attachedInvestigator ?~ iid)
    EndOfGame | fromJustNote "Must be set" treacheryClues > 0 ->
      let
        investigator =
          fromJustNote "missing investigator" treacheryAttachedInvestigator
      in t <$ unshiftMessage (SufferTrauma investigator 0 1)
    UseCardAbility iid (TreacherySource tid) _ 1 | tid == treacheryId -> do
      cluesToRemove <- withQueue $ \queue -> do
        let
          (before, after) = flip break queue $ \case
            DiscoverClues{} -> True
            _ -> False
          (DiscoverClues _ _ m) = case after of
            [] -> error "DiscoverClues has to be present"
            (x : _) -> x
          remaining = case after of
            [] -> []
            (_ : xs) -> xs
        (before <> remaining, m)
      let
        remainingClues =
          max 0 (fromJustNote "Must be set" treacheryClues - cluesToRemove)
      if remainingClues == 0
        then do
          unshiftMessage
            (RemoveAllModifiersOnTargetFrom
              (InvestigatorTarget iid)
              (TreacherySource treacheryId)
            )
          pure $ CoverUp $ attrs { treacheryClues = Just remainingClues }
        else pure $ CoverUp (attrs { treacheryClues = Just remainingClues })
    _ -> CoverUp <$> runMessage msg attrs
