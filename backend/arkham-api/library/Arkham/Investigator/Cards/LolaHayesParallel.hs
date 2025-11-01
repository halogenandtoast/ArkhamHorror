module Arkham.Investigator.Cards.LolaHayesParallel (lolaHayesParallel) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.ClassSymbol
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype LolaHayesParallel = LolaHayesParallel InvestigatorAttrs
  deriving anyclass IsInvestigator
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

lolaHayesParallel :: InvestigatorCard LolaHayesParallel
lolaHayesParallel =
  investigator LolaHayesParallel Cards.lolaHayesParallel
    $ Stats {health = 6, sanity = 6, willpower = 3, intellect = 3, combat = 3, agility = 3}

instance HasModifiersFor LolaHayesParallel where
  getModifiersFor (LolaHayesParallel attrs) = do
    mLeadingLady <- getMeta attrs "leadingLady"
    modifySelf attrs $ case mLeadingLady of
      Nothing ->
        [ CannotPlay $ not_ $ mapOneOf CardWithClass $ nub [Neutral, investigatorClass attrs]
        , CannotCommitCards $ not_ $ mapOneOf CardWithClass $ nub [Neutral, investigatorClass attrs]
        ]
      Just cid ->
        [ CannotPlay $ not_ $ oneOf $ CardWithId cid
            : map CardWithClass (nub [Neutral, investigatorClass attrs])
        , CannotCommitCards $ not_ $ oneOf $ CardWithId cid
            : map CardWithClass (nub [Neutral, investigatorClass attrs])
        ]
    msamuel <-
      selectOne $ inHandOf NotForPlay attrs.id <> basic (cardIs Assets.samuelBlakeObsessiveProducer)
    for_ msamuel \samuel ->
      modified_
        samuel.id
        attrs
        [CannotPlay $ not_ $ mapOneOf CardWithClass $ nub [Neutral, investigatorClass attrs]]

instance HasAbilities LolaHayesParallel where
  getAbilities (LolaHayesParallel a) =
    [ selfAbility
        a
        1
        (not_ $ exists $ inHandOf NotForPlay a.id <> basic (cardIs Assets.samuelBlakeObsessiveProducer))
        $ forced
        $ TurnBegins #when You
    ]

instance HasChaosTokenValue LolaHayesParallel where
  getChaosTokenValue iid ElderSign (LolaHayesParallel attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 2)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

switchRole :: ReverseQueue m => InvestigatorAttrs -> m ()
switchRole attrs = do
  let roles = filter (/= Mythos) [minBound .. maxBound]
  msamuel <- select $ assetIs Assets.samuelBlakeObsessiveProducer
  chooseOneM attrs.id $ for_ roles \role ->
    labeled (tshow role) do
      push $ SetRole attrs.id role
      for_ msamuel \samuel ->
        when (role /= investigatorClass attrs) do
          assignHorror attrs.id samuel 1

instance RunMessage LolaHayesParallel where
  runMessage msg i@(LolaHayesParallel attrs) = runQueueT $ case msg of
    ForInvestigator iid BeginGame | iid == attrs.id -> do
      attrs' <- liftRunMessage msg attrs
      pure $ LolaHayesParallel $ attrs' {investigatorClass = Neutral}
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      switchRole attrs
      pure i
    ElderSignEffect iid | attrs `is` iid -> do
      ok <- selectNone $ inHandOf NotForPlay iid <> basic (cardIs Assets.samuelBlakeObsessiveProducer)
      when ok $ switchRole attrs
      pure i
    _ -> LolaHayesParallel <$> liftRunMessage msg attrs
