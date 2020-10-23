{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.CoverUp
  ( CoverUp(..)
  , coverUp
  )
where

import Arkham.Import

import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype CoverUp = CoverUp Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

coverUp :: TreacheryId -> Maybe InvestigatorId -> CoverUp
coverUp uuid iid =
  CoverUp $ (weaknessAttrs uuid iid "01007") { treacheryClues = Just 3 }

coverUpClues :: Attrs -> Int
coverUpClues Attrs { treacheryClues } =
  fromJustNote "must be set" treacheryClues

instance HasModifiersFor env CoverUp where
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env CoverUp where
  getActions iid window@(WhenDiscoverClues You YourLocation) (CoverUp a@Attrs {..})
    | Just iid == treacheryAttachedInvestigator
    = do
      cluesToDiscover <- fromQueue $ \queue -> do
        let
          mDiscoverClues = flip find queue $ \case
            DiscoverClues{} -> True
            _ -> False
        case mDiscoverClues of
          Just (DiscoverClues _ _ m) -> m
          _ -> 0
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility (toSource a) 1 (ReactionAbility window))
        | coverUpClues a > 0 && cluesToDiscover > 0
        ]
  getActions _ _ _ = pure []

instance (TreacheryRunner env) => RunMessage env CoverUp where
  runMessage msg t@(CoverUp attrs@Attrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      unshiftMessages
        [ RemoveCardFromHand iid "01007"
        , AttachTreachery treacheryId (InvestigatorTarget iid)
        ]
      CoverUp <$> runMessage msg (attrs & attachedInvestigator ?~ iid)
    InvestigatorEliminated iid | ownedBy attrs iid -> do
      runMessage EndOfGame t >>= \case
        CoverUp attrs' -> CoverUp <$> runMessage msg attrs'
    EndOfGame | coverUpClues attrs > 0 ->
      let
        investigator =
          fromJustNote "missing investigator" treacheryAttachedInvestigator
      in t <$ unshiftMessage (SufferTrauma investigator 0 1)
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      cluesToRemove <- withQueue $ \queue -> do
        let
          (before, after) = flip break queue $ \case
            DiscoverClues{} -> True
            _ -> False
          (DiscoverClues _ _ m, remaining) = case after of
            [] -> error "DiscoverClues has to be present"
            (x : xs) -> (x, xs)
        (before <> remaining, m)
      let remainingClues = max 0 (coverUpClues attrs - cluesToRemove)
      when (remainingClues == 0) $ unshiftMessage
        (RemoveAllModifiersOnTargetFrom (InvestigatorTarget iid) source)
      pure $ CoverUp (attrs { treacheryClues = Just remainingClues })
    _ -> CoverUp <$> runMessage msg attrs
