.PHONY: start exit

bernado_setup:
	@echo "Setting up container and Git branch for user Julian..."
	@docker run -d -p 8786:8787 -e PASSWORD=tea --name bernado_urps25 urps25
	@echo "Setting up Git branch"
	@git checkout bernado 2>/dev/null || git checkout -b bernado

bernado_start:
	@echo "Establishing SSH tunnel and setting up remote environment for container Julian..."
	ssh -L 8786:localhost:8786 -J evalengin.isr.umich.edu cochran.isr.umich.edu << EOF
	cd urps-modelling
	git checkout bernado
	git pull origin bernado
	docker start bernado_urps25
	docker cp ./ bernado_urps25:/home/rstudio/urps-modelling
	docker cp ../../tea/bernado/TEA_2019.csv bernado_urps25:/home/rstudio/TEA_2019.csv
EOF

bernado_exit:
	@echo "Performing cleanup and saving changes for container Julian..."
	@rm -rf ../docker
	@mkdir ../docker
	@echo "Copying data from container bernado_urps25..."
	@docker cp bernado_urps25:/home/rstudio/urps-modelling ../docker/urps-modelling
	@docker stop bernado_urps25
	@echo "Updating Git repository on branch 'bernado'..."
	@cd ../docker && \
		git checkout bernado && \
		git add .; \
		read -p "Enter commit message: " msg; \
		git commit -m "$$msg"; \
		git push origin bernado
	@exit

ckmoy_setup:
	@echo "Setting up container and Git branch for user Caroline..."
	@docker run -d -p 8787:8787 -e PASSWORD=tea --name ckmoy_urps25 urps25
	@echo "Setting up Git branch"
	@git checkout ckmoy 2>/dev/null || git checkout -b ckmoy

ckmoy_start:
	@echo "Establishing SSH tunnel and setting up remote environment for container Caroline..."
	ssh -L 8787:localhost:8787 -J ckmoy@evalengin.isr.umich.edu ckmoy@cochran.isr.umich.edu << EOF
	cd urps-modelling
	git checkout ckmoy
	git pull origin ckmoy
	docker start ckmoy_urps25
	docker cp ./ ckmoy_urps25:/home/rstudio/urps-modelling
	docker cp ../../tea/ckmoy/TEA_2019.csv ckmoy_urps25:/home/rstudio/TEA_2019.csv
EOF

ckmoy_exit:
	@echo "Performing cleanup and saving changes for container ckmoy_urps25..."
	@rm -rf ../docker
	@mkdir ../docker
	@echo "Copying data from container ckmoy_urps25..."
	@docker cp ckmoy_urps25:/home/rstudio/urps-modelling ../docker/urps-modelling
	@docker stop ckmoy_urps25
	@echo "Updating Git repository on branch 'ckmoy'..."
	@cd ../docker && \
		git checkout ckmoy && \
		git add .; \
		read -p "Enter commit message: " msg; \
		git commit -m "$$msg"; \
		git push origin ckmoy
	@exit

zjunjie_setup:
	@echo "Setting up container and Git branch for user Junjie..."
	@docker run -d -p 8788:8787 -e PASSWORD=tea --name zjunjie_urps25 urps25
	@echo "Setting up Git branch"
	@git checkout zjunjie 2>/dev/null || git checkout -b zjunjie

zjunjie_start:
	@echo "Establishing SSH tunnel and setting up remote environment for container zjunjie_urps25..."
	ssh -L 8788:localhost:8788 -J zjunjie@evalengin.isr.umich.edu zjunjie@cochran.isr.umich.edu << EOF
	cd urps-modelling
	git checkout zjunjie
	git pull origin zjunjie
	docker start zjunjie_urps25
	docker cp ./ zjunjie_urps25:/home/rstudio/urps-modelling
	docker cp ../../tea/zjunjie/TEA_2019.csv zjunjie_urps25:/home/rstudio/TEA_2019.csv
EOF

zjunjie_exit:
	@echo "Performing cleanup and saving changes for container Junjie..."
	@rm -rf ../docker
	@mkdir ../docker
	@echo "Copying data from container zjunjie_urps25..."
	@docker cp zjunjie_urps25:/home/rstudio/urps-modelling ../docker/urps-modelling
	@docker stop zjunjie_urps25
	@echo "Updating Git repository on branch 'zjunjie'..."
	@cd ../docker && \
		git checkout zjunjie && \
		git add .; \
		read -p "Enter commit message: " msg; \
		git commit -m "$$msg"; \
		git push origin zjunjie
	@exit